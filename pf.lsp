

(defun eliza--space-p (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defparameter *eliza-punct-chars* ".,;:()?!\"'")

(defun clean-and-tokenize (line)
  (when line
    (let* ((lower (string-downcase line))
           (clean-str
            (with-output-to-string (out)
              (loop for c across lower do
                    (if (find c *eliza-punct-chars* :test #'char-equal)
                        (write-char #\Space out)
                        (write-char c out))))))
      (let ((tokens '())
            (cur ""))
        (loop for c across clean-str do
              (if (eliza--space-p c)
                  (when (> (length cur) 0)
                    (push cur tokens)
                    (setf cur ""))
                  (setf cur (concatenate 'string cur (string c)))))
        (when (> (length cur) 0) (push cur tokens))
        (nreverse tokens)))))

(defun element-match-p (templ-el token)
  (cond
    ((null templ-el) t)
    ((and (consp templ-el)
          (symbolp (first templ-el))
          (string-equal (symbol-name (first templ-el)) "s"))
     (not (null token)))
    ((symbolp templ-el)
     (and token (string-equal (symbol-name templ-el) token)))
    (t nil)))

(defun match-template (stim input)
  (labels ((rec (slist ilist)
             (cond
               ((null slist) t)
               ((null ilist) nil)
               (t
                (let ((se (first slist))
                      (it (first ilist)))
                  (if (element-match-p se it)
                      (rec (rest slist) (rest ilist))
                      nil))))))
    (rec stim input)))

(defun get-token-at (input pos)
  (if (and input (>= pos 0) (< pos (length input)))
      (nth pos input)
      ""))

(defun build-response-from-resp (resp indices input)
  (mapcan
   (lambda (e)
     (cond
       ((integerp e)
        (let ((idx (and indices (nth e indices))))
          (list (get-token-at input (or idx -1)))))
       ((symbolp e) (list (symbol-name e)))
       ((stringp e) (list e))
       (t (list (princ-to-string e)))))
   resp))

;;; ============================================================================
;;; 2. Bases de Conocimiento (Datos Migrados de Prolog)
;;; ============================================================================

(defparameter *sintomas-usuario* nil)

(defparameter *sintomas-db*
  '(("hepatitis" "ictericia" "orina_oscura" "fatiga_extrema" "dolor_abdominal" "perdida_apetito" "nauseas")
    ("cancer_cervicouterino" "sangrado_anormal" "dolor_pelvico" "flujo_inusual" "dolor_relaciones" "perdida_peso" "fatiga")
    ("hipertiroidismo" "taquicardia" "perdida_peso_involuntaria" "ansiedad" "temblores" "intolerancia_calor" "ojos_saltones" "aumento_apetito")))

(defparameter *tratamientos-db*
  '(("hepatitis" "Reposo, hidratacion, antivirales y seguimiento con Hepatologo o Gastroenterologo.")
    ("cancer_cervicouterino" "Cirugia, radioterapia o quimioterapia segun etapa. Atiende Oncologo Ginecologo.")
    ("hipertiroidismo" "Medicamentos antitiroideos, yodo radioactivo o cirugia. Atiende Endocrinologo.")))

(defparameter *recomendaciones-db*
  '(("hepatitis"
     ("Leve" . "Reposo en casa, evitar alcohol y grasas.")
     ("Moderada" . "Monitoreo de funcion hepatica y medicamentos antivirales.")
     ("Severa" . "Hospitalizacion inmediata por riesgo de falla hepatica."))
    ("cancer_cervicouterino"
     ("Leve" . "Seguimiento colposcopico y posible escision local.")
     ("Moderada" . "Intervencion quirurgica y evaluacion de ganglios.")
     ("Severa" . "Tratamiento oncologico agresivo y cuidados paliativos si aplica."))
    ("hipertiroidismo"
     ("Leve" . "Uso de betabloqueadores para controlar sintomas cardiacos.")
     ("Moderada" . "Tratamiento con metimazol y control hormonal estricto.")
     ("Severa" . "Yodo radioactivo o tiroidectomia urgente por tormenta tiroidea."))))

(defparameter *gravedad-db*
  '(("hepatitis" "medio")
    ("cancer_cervicouterino" "alto")
    ("hipertiroidismo" "medio")))

(defparameter *contradictorios-db*
  '(("perdida_apetito" "aumento_apetito")
    ("intolerancia_calor" "escalofrios")
    ("perdida_peso" "aumento_peso")))

;; --- Datos Star Wars y Familia ---


(defparameter *starwars-desc-db*
  '(("anakin" "fue" "un" "jedi" "que" "cayo" "al" "lado" "oscuro")
    ("luke" "es" "un" "maestro" "jedi" "hijo" "de" "anakin")
    ("leia" "es" "lider" "de" "la" "resistencia" "y" "hermana" "de" "luke")
    ("han" "es" "un" "famoso" "contrabandista" "y" "piloto")
    ("ben" "es" "kylo" "ren" "hijo" "de" "han" "y" "leia")
    ("yoda" "es" "un" "sabio" "maestro" "jedi")
    ("obiwan" "es" "el" "maestro" "de" "anakin")
    ("palpatine" "es" "el" "emperador" "sith")
    ("chewbacca" "es" "un" "wookiee" "leal")
    ("rey" "es" "una" "jedi" "muy" "poderosa")
    ("mando" "es" "un" "cazarrecompensas" "mandaloriano")
    ("ahsoka" "es" "una" "togruta" "ex" "jedi")))

;; Grupos
(defparameter *grupo-starwars*
  '("anakin" "luke" "leia" "han" "ben" "yoda" "obiwan" "palpatine" "chewbacca" "rey" "mando" "ahsoka" "padme" "shmi"))

(defparameter *grupo-familia*
  '("donrafael" "rafael" "donsergio" "sergio" "mauricio" "donpatricio" "patricio" "joseluis" "german" "orlando" "marcelo" "alfonso" "rodrigo" "checo" "emilio" "catalina" "carmen" "lourdes" "alejandra" "vicky" "caty" "gina" "carmelita" "claudia" "ana" "julia" "mariana" "auxilio" "isabella" "casandra" "camila" "luisa" "gisel" "victoria"))

(defparameter *adultos-db*
  '("anakin" "padme" "obiwan" "yoda" "palpatine" "han" "mando"
    "donrafael" "rafael" "donsergio" "sergio" "mauricio" "donpatricio" "patricio" "joseluis" "german" "orlando" "alfonso" "rodrigo" "rafa" "gisel" "carmen" "lourdes" "catalina" "alejandra" "vicky" "caty" "gina" "carmelita" "claudia" "ana" "julia" "mariana" "auxilio"))

(defparameter *hombres-db*
  '("donrafael" "rafael" "donsergio" "sergio" "mauricio" "donpatricio" "patricio" "joseluis" "german" "rafa" "orlando" "marcelo" "alfonso" "rodrigo" "checo" "emilio"
    "anakin" "luke" "han" "ben" "obiwan" "yoda" "palpatine" "chewbacca" "mando" "finn" "poe"))

(defparameter *mujeres-db*
  '("shmi" "padme" "leia" "rey" "ahsoka"
    "carmen" "auxilio" "lourdes" "alejandra" "vicky" "caty" "gina" "mariana" "ana" "julia" "victoria" "isabella" "casandra" "camila" "claudia" "carmelita" "gisel" "catalina" "luisa"))


(defparameter *padre-db*
  '(
    ("anakin" "luke") ("anakin" "leia") ("han" "ben")
    
    ("donrafael" "rafael") ("donrafael" "carmen") ("donrafael" "auxilio") ("donrafael" "lourdes") ("donrafael" "joseluis") ("donrafael" "alejandra") ("donrafael" "german")
    ("rafael" "rafa") ("rafael" "ana")
    ("donsergio" "sergio") ("donsergio" "mauricio")
    ("donpatricio" "gisel") ("donpatricio" "patricio")
    ("joseluis" "alfonso") ("joseluis" "luisa")
    ("german" "rodrigo") ("german" "julia")
    ("rafa" "victoria")
    ("sergio" "checo") ("sergio" "isabella")
    ("orlando" "marcelo") ("orlando" "casandra")))

(defparameter *madre-db*
  '(
    ("shmi" "anakin") ("padme" "luke") ("padme" "leia") ("leia" "ben")
   
    ("catalina" "rafael") ("catalina" "carmen") ("catalina" "auxilio") ("catalina" "lourdes") ("catalina" "joseluis") ("catalina" "alejandra") ("catalina" "german")
    ("carmen" "mauricio") ("carmen" "sergio")
    ("lourdes" "patricio") ("lourdes" "gisel")
    ("alejandra" "mariana")
    ("vicky" "rafa") ("vicky" "ana")
    ("caty" "alfonso") ("caty" "luisa")
    ("gina" "rodrigo") ("gina" "julia")
    ("gisel" "casandra") ("gisel" "marcelo")
    ("ana" "camila") ("ana" "emilio")
    ("carmelita" "victoria")
    ("claudia" "checo") ("claudia" "isabella")))


(defparameter *likes-db* '("apples" "ponies" "zombies" "manzanas" "computadoras" "carros"))
(defparameter *does-db* '("study" "cook" "work"))
(defparameter *is-db* '("dumb" "weird" "nice" "fine" "happy" "redundant"))


;;; ============================================================================
;;; 3. Logica Medica (Implementacion de Actividades)
;;; ============================================================================

(defun get-sintomas-enfermedad (enf)
  (cdr (assoc enf *sintomas-db* :test #'string-equal)))

(defun count-sintomas-presentes (enf)
  (let ((sints (get-sintomas-enfermedad enf)))
    (count-if (lambda (s) (find s *sintomas-usuario* :test #'string-equal)) sints)))

(defun get-probabilidad (enf)
  (let* ((sints (get-sintomas-enfermedad enf))
         (total (length sints))
         (presentes (count-if (lambda (s) (find s *sintomas-usuario* :test #'string-equal)) sints)))
    (if (> total 0)
        (* (/ presentes total) 100.0)
        0.0)))

(defun get-severity (enf)
  (let ((count (count-sintomas-presentes enf)))
    (cond ((>= count 3) "Severa")
          ((= count 2) "Moderada")
          ((= count 1) "Leve")
          (t nil))))

(defun get-arbol-diagnostico ()
  "Devuelve lista de enfermedades donde se cumplen TODOS los sintomas."
  (let ((diagnosticos '()))
    (dolist (entry *sintomas-db*)
      (let* ((enf (car entry))
             (sints (cdr entry)))
        (when (every (lambda (s) (find s *sintomas-usuario* :test #'string-equal)) sints)
          (push enf diagnosticos))))
    diagnosticos))

(defun get-diagnostico-preventivo ()
  "Enfermedades con al menos 1 sintoma pero no todos."
  (let ((preventivos '()))
    (dolist (entry *sintomas-db*)
      (let* ((enf (car entry))
             (sints (cdr entry))
             (total (length sints))
             (presentes (count-if (lambda (s) (find s *sintomas-usuario* :test #'string-equal)) sints)))
        (when (and (> presentes 0) (< presentes total))
          (push enf preventivos))))
    preventivos))

(defun check-similares (e1 e2)
  (let ((s1 (get-sintomas-enfermedad e1))
        (s2 (get-sintomas-enfermedad e2)))
    (let ((shared (intersection s1 s2 :test #'string-equal)))
      (>= (length shared) 2))))

(defun check-contradictorios ()
  (loop for pair in *contradictorios-db*
        when (and (find (first pair) *sintomas-usuario* :test #'string-equal)
                  (find (second pair) *sintomas-usuario* :test #'string-equal))
        return t))

(defun get-diagnostico-exclusivo ()
  "Busca si hay un sintoma unico que determine una enfermedad."
  (let ((unico nil))
    (dolist (s *sintomas-usuario*)
      ;; Buscar cuantas enfermedades tienen este sintoma
      (let ((enfs-con-s (remove-if-not (lambda (entry) (find s (cdr entry) :test #'string-equal)) *sintomas-db*)))
        (when (= (length enfs-con-s) 1)
          (setf unico (list (car (first enfs-con-s)) s))
          (return))))
    unico))

(defun get-tratamiento-combinado ()
  "Tratamientos para todas las enfermedades con al menos 1 sintoma."
  (let ((trats '()))
    (dolist (entry *sintomas-db*)
      (let ((enf (car entry)))
        (when (> (count-sintomas-presentes enf) 0)
          (let ((t-text (second (assoc enf *tratamientos-db* :test #'string-equal))))
            (when t-text (pushnew t-text trats :test #'string-equal))))))
    trats))

;;; ============================================================================
;;; 4. Logica Genealogica (Reglas de parentesco)
;;; ============================================================================
(defun get-parents (child)
  (let ((parents '()))
    (dolist (pair *padre-db*) (if (string-equal (second pair) child) (push (first pair) parents)))
    (dolist (pair *madre-db*) (if (string-equal (second pair) child) (push (first pair) parents)))
    parents))

(defun get-children (parent)
  (let ((children '()))
    (dolist (pair *padre-db*) (if (string-equal (first pair) parent) (push (second pair) children)))
    (dolist (pair *madre-db*) (if (string-equal (first pair) parent) (push (second pair) children)))
    children))

(defun get-siblings (person)
  (let ((parents (get-parents person)) (siblings '()))
    (dolist (p parents)
      (dolist (c (get-children p))
        (unless (string-equal c person) (pushnew c siblings :test #'string-equal))))
    siblings))

(defun get-grandparents (person)
  (let ((parents (get-parents person)) (grandparents '()))
    (dolist (p parents)
      (dolist (gp (get-parents p)) (pushnew gp grandparents :test #'string-equal)))
    grandparents))

(defun get-uncles (person)
  (let ((parents (get-parents person)) (uncles '()))
    (dolist (p parents)
      (dolist (u (get-siblings p)) (pushnew u uncles :test #'string-equal)))
    uncles))

(defun get-nephews (person)
  (let ((siblings (get-siblings person)) (nephews '()))
    (dolist (s siblings)
      (dolist (n (get-children s)) (pushnew n nephews :test #'string-equal)))
    nephews))

(defun get-cousins (person)
  (let ((uncles (get-uncles person)) (cousins '()))
    (dolist (u uncles)
      (dolist (c (get-children u)) (pushnew c cousins :test #'string-equal)))
    cousins))

;;; ============================================================================
;;; 5. Templates 
;;; ============================================================================

(defparameter *templates*
  (list
   ;; Saludos
   (list (list 'hola 'mi 'nombre 'es (list 's)) (list "Hola" 0 "¿" "Cómo" "estás" "tú" "?") (list 4))
   (list (list 'buendia 'mi 'nombre 'es (list 's)) (list "Buen" "día" 0 "¿" "Cómo" "estás" "tú" "?") (list 4))
   (list (list 'buenos 'dias 'mi 'nombre 'es (list 's)) (list "Buenos" "días" 0 "¿" "Cómo" "estás" "tú" "?") (list 5))
   (list (list 'hola (list 's)) (list "Hola" "¿" "Cómo" "estás" "tú" "?") nil)
   (list (list 'buendia (list 's)) (list "Buen" "día" "¿" "Cómo" "estás" "tú" "?") nil)
   (list (list 'buenos 'dias (list 's)) (list "Buenos" "días" "¿" "Cómo" "estás" "tú" "?") nil)
   (list (list 'hi (list 's)) (list "Hola" "¿" "Cómo" "estás" "tú" "?") nil)
   (list (list 'hello (list 's)) (list "Hola" "¿" "Cómo" "estás" "tú" "?") nil)

   (list (list 'yo 'creo 'que 'soy (list 's)) (list "¿" "Por" "qué" "crees" "que" "eres" 0 "?") (list 4))
   (list (list 'yo (list 's) 'a 'ti) (list "¿" "Por" "qué" "me" 0 "?") (list 1))
   (list (list 'yo 'soy (list 's)) (list "¿" "Por" "qué" "eres" 0 "?") (list 2))


   (list (list 'te 'gustan 'las (list 's) (list 's)) (list 'flagLike) (list 3))
   (list (list 'tu 'eres (list 's) (list 's)) (list 'flagDo) (list 2))
   (list (list 'que 'eres 'tu (list 's)) (list 'flagIs) (list 3))

   (list (list 'quien 'es (list 's)) (list 'flagPersonaje) (list 2))
   (list (list 'conoces 'a (list 's)) (list 'flagPersonaje) (list 2))
   (list (list 'hablame 'de (list 's)) (list 'flagPersonaje) (list 2))
   (list (list 'sabes 'quien 'es (list 's)) (list 'flagPersonaje) (list 3))
   (list (list (list 's) 'es 'adulto) (list 'flagAdulto) (list 0))
   (list (list 'es (list 's) 'un 'adulto) (list 'flagAdulto) (list 1))

   (list (list 'tengo (list 's)) (list 'flagAddSintoma) (list 1))
   (list (list 'siento (list 's)) (list 'flagAddSintoma) (list 1))
   (list (list 'padezco (list 's)) (list 'flagAddSintoma) (list 1))
   (list (list 'presento (list 's)) (list 'flagAddSintoma) (list 1))
   (list (list 'borrar 'sintomas) (list 'flagClearSintomas) nil)
   (list (list 'limpiar 'sintomas) (list 'flagClearSintomas) nil)
   (list (list 'reiniciar 'paciente) (list 'flagClearSintomas) nil)
   (list (list 'nuevo 'paciente) (list 'flagClearSintomas) nil)

   (list (list 'cuales 'son 'los 'sintomas 'de (list 's)) (list 'flagSintomas) (list 5))
   (list (list 'que 'sintomas 'tiene (list 's)) (list 'flagSintomas) (list 3))
   (list (list 'como 'se 'manifiesta 'la (list 's)) (list 'flagSintomas) (list 4))
   (list (list 'que 'se 'siente 'con (list 's)) (list 'flagSintomas) (list 4))
   (list (list 'como 'saber 'si 'tengo (list 's)) (list 'flagSintomas) (list 4))
   (list (list 'signos 'de (list 's)) (list 'flagSintomas) (list 2))

   (list (list 'cual 'es 'el 'tratamiento 'de (list 's)) (list 'flagTratamiento) (list 5))
   (list (list 'como 'se 'cura 'la (list 's)) (list 'flagTratamiento) (list 4))
   (list (list 'que 'puedo 'tomar 'para (list 's)) (list 'flagTratamiento) (list 4))
   (list (list 'remedio 'para (list 's)) (list 'flagTratamiento) (list 2))
   (list (list 'cura 'para (list 's)) (list 'flagTratamiento) (list 2))

   (list (list 'es 'grave 'la (list 's)) (list 'flagGravedad) (list 3))
   (list (list 'que 'gravedad 'tiene 'la (list 's)) (list 'flagGravedad) (list 4))
   (list (list 'que 'gravedad 'tiene 'el (list 's)) (list 'flagGravedad) (list 4))
   (list (list 'es 'grave 'el (list 's)) (list 'flagGravedad) (list 3))

   (list (list 'que 'me 'recomiendas 'para (list 's)) (list 'flagRecomendacion) (list 4))
   (list (list 'que 'hago 'si 'tengo (list 's)) (list 'flagRecomendacion) (list 4))
   (list (list 'dame 'una 'recomendacion 'para (list 's)) (list 'flagRecomendacion) (list 5))

   (list (list 'probabilidad 'de (list 's)) (list 'flagCalcularProbabilidad) (list 2))
   (list (list 'que 'probabilidad 'tengo 'de (list 's)) (list 'flagCalcularProbabilidad) (list 4))
   (list (list 'posibilidad 'de (list 's)) (list 'flagCalcularProbabilidad) (list 2))

   (list (list 'diagnostico 'preventivo) (list 'flagDiagnosticoPreventivo) nil)
   (list (list 'alerta 'preventiva) (list 'flagDiagnosticoPreventivo) nil)
   (list (list 'que 'podria 'tener) (list 'flagDiagnosticoPreventivo) nil)

   (list (list 'es (list 's) 'similar 'a (list 's)) (list 'flagSimilares) (list 1 4))
   (list (list 'se 'parecen 'la (list 's) 'y 'la (list 's)) (list 'flagSimilares) (list 3 6))
   (list (list 'relacion 'entre (list 's) 'y (list 's)) (list 'flagSimilares) (list 2 4))

   (list (list 'tengo 'sintomas 'contradictorios) (list 'flagContradictorios) nil)
   (list (list 'hay 'contradiccion) (list 'flagContradictorios) nil)
   (list (list 'mis 'sintomas 'tienen 'sentido) (list 'flagContradictorios) nil)

   (list (list 'diagnostico 'exacto) (list 'flagArbol) nil)
   (list (list 'diagnostico 'definitivo) (list 'flagArbol) nil)
   (list (list 'que 'tengo 'exactamente) (list 'flagArbol) nil)

   (list (list 'diagnostico 'exclusivo) (list 'flagDiagnosticoExclusivo) nil)
   (list (list 'tengo 'algun 'sintoma 'unico) (list 'flagDiagnosticoExclusivo) nil)

   (list (list 'riesgo 'de (list 's)) (list 'flagRiesgo) (list 2))
   (list (list 'es 'peligrosa 'la (list 's)) (list 'flagRiesgo) (list 3))

   (list (list 'tratamiento 'combinado) (list 'flagTratamientoCombinado) nil)
   (list (list 'todos 'los 'tratamientos) (list 'flagTratamientoCombinado) nil)

   (list (list 'recomendacion 'severidad (list 's)) (list 'flagRecomendacionSeveridad) (list 2))
   (list (list 'que 'tan 'grave 'es 'mi (list 's)) (list 'flagRecomendacionSeveridad) (list 5))

   (list (list 'diagnosticar 'y 'tratar) (list 'flagDiagnosticarYTratar) nil)
   (list (list 'dame 'el 'remedio) (list 'flagDiagnosticarYTratar) nil)

   (list (list 'reporte 'completo) (list 'flagReporte) nil)
   (list (list 'resumen) (list 'flagReporte) nil)
   (list (list 'informe) (list 'flagReporte) nil)

   (list (list 'diagnosticame 'por 'probabilidad) (list 'flagDiagnosticoProbabilidad) nil)
   (list (list 'diagnosticame) (list 'flagDiagnostico) nil)

   ;; Familia
   (list (list 'quien 'es 'el 'padre 'de (list 's)) (list 'flagPadre) (list 5))
   (list (list 'como 'se 'llama 'el 'papa 'de (list 's)) (list 'flagPadre) (list 6))
   (list (list 'quien 'es 'papa 'de (list 's)) (list 'flagPadre) (list 4))
   (list (list 'quien 'es 'la 'madre 'de (list 's)) (list 'flagMadre) (list 5))
   (list (list 'como 'se 'llama 'la 'mama 'de (list 's)) (list 'flagMadre) (list 6))
   (list (list 'quien 'es 'mama 'de (list 's)) (list 'flagMadre) (list 4))
   (list (list 'quien 'es 'hijo 'de (list 's)) (list 'flagHijo) (list 4))
   (list (list 'cuales 'son 'los 'hijos 'de (list 's)) (list 'flagHijo) (list 5))
   (list (list 'quien 'es 'hermano 'de (list 's)) (list 'flagHermano) (list 4))
   (list (list 'quien 'es 'la 'hermana 'de (list 's)) (list 'flagHermano) (list 5))
   (list (list 'quienes 'son 'los 'hermanos 'de (list 's)) (list 'flagHermano) (list 5))
   (list (list 'quien 'es 'abuelo 'de (list 's)) (list 'flagAbuelo) (list 4))
   (list (list 'quien 'es 'tio 'de (list 's)) (list 'flagTio) (list 4))
   (list (list 'quien 'es 'sobrino 'de (list 's)) (list 'flagSobrino) (list 4))
   (list (list 'quien 'es 'primo 'de (list 's)) (list 'flagPrimo) (list 4))
   (list (list 'quienes 'son 'personajes 'de (list 's)) (list 'flagPersonajeDe) (list 4))
   (list (list 'cuantos (list 's) 'hay 'en 'la 'familia) (list 'flagContar) (list 1))
   (list (list 'cuantas (list 's) 'hay 'en 'la 'familia) (list 'flagContar) (list 1))
   (list (list 'cuantos (list 's) 'hay 'en 'star 'wars) (list 'flagContarStarWars) (list 1))
   (list (list 'cuantas (list 's) 'hay 'en 'star 'wars) (list 'flagContarStarWars) (list 1))

   ;; Fallback
   (list nil (list "Please" "explain" "a" "little" "more" ".") nil)))

(defun find-matching-template (input)
  (find-if (lambda (tpl) (match-template (first tpl) input)) *templates*))

;;; ============================================================================
;;; 6. Manejo de Flags
;;; ============================================================================

(defun handle-flag (flag indices input)
  (let ((arg-token (and indices (> (length indices) 0) (get-token-at input (first indices))))
        (arg-token2 (and indices (> (length indices) 1) (get-token-at input (second indices)))))
    (case flag
      ;; --- Generales ---
      (flagLike (if (find arg-token *likes-db* :test #'string-equal)
                    (list "Sí" "me" "gusta" arg-token)
                    (list "No" "no" "me" "gusta" arg-token)))
      (flagDo   (if (find arg-token *does-db* :test #'string-equal)
                    (list "Sí" "yo" arg-token "y" "me" "encanta")
                    (list "No" "yo" "no" arg-token "." "es" "muy" "difícil" "para" "mí")))
      (flagIs   (if (find arg-token *is-db* :test #'string-equal)
                    (list "Sí" "yo" "soy" arg-token)
                    (list "No" "yo" "no" "soy" arg-token)))
      
      (flagPersonaje 
       (let ((entry (assoc arg-token *starwars-desc-db* :test #'string-equal)))
         (if entry (append (list arg-token) (cdr entry))
             (list "Lo" "siento" "no" "conozco" "a" arg-token))))

      (flagAdulto (if (find arg-token *adultos-db* :test #'string-equal)
                      (list "Si" arg-token "es" "un" "adulto")
                      (list "No" arg-token "no" "es" "un" "adulto")))

      (flagAddSintoma
       (pushnew arg-token *sintomas-usuario* :test #'string-equal)
       (list "Entendido" "," "he" "registrado" "que" "tienes" arg-token))

      (flagClearSintomas
       (setf *sintomas-usuario* nil)
       (list "Síntomas" "borrados" "correctamente" "."))

      (flagSintomas
       (let ((sints (get-sintomas-enfermedad arg-token)))
         (if sints (append (list "Los" "síntomas" "de" arg-token "son" ":") sints)
             (list "No" "tengo" "información" "sobre" "los" "síntomas" "de" arg-token))))

      (flagTratamiento
       (let ((entry (assoc arg-token *tratamientos-db* :test #'string-equal)))
         (if entry (list "El" "tratamiento" "para" arg-token "es" ":" (second entry))
             (list "No" "conozco" "el" "tratamiento" "para" arg-token))))

      (flagRecomendacion
       (let ((entry (assoc arg-token *recomendaciones-db* :test #'string-equal)))
         (if entry
             (let ((recs (mapcar #'cdr (cdr entry))))
               (append (list "Recomendaciones" "para" arg-token ":") recs))
             (list "No" "tengo" "recomendaciones" "para" arg-token))))

      (flagGravedad
       (let ((entry (assoc arg-token *gravedad-db* :test #'string-equal)))
         (if entry (list "La" "gravedad" "de" "la" arg-token "es" ":" (second entry))
             (list "No" "tengo" "informacion" "sobre" "la" "gravedad" "de" arg-token))))

      (flagCalcularProbabilidad
       (let ((prob (get-probabilidad arg-token)))
         (list "La" "probabilidad" "de" "tener" arg-token "es" "del" (format nil "~,2f%" prob))))

      (flagDiagnosticoPreventivo
       (let ((prev (get-diagnostico-preventivo)))
         (if prev (append (list "Atención" ":" "podrías" "estar" "desarrollando" ":") prev)
             (list "No" "hay" "alertas" "de" "diagnóstico" "preventivo" "por" "ahora"))))

      (flagSimilares
       (if (check-similares arg-token arg-token2)
           (list "Sí" arg-token "y" arg-token2 "son" "enfermedades" "similares" "porque" "comparten" "síntomas")
           (list "No" arg-token "y" arg-token2 "no" "se" "consideran" "similares")))

      (flagContradictorios
       (if (check-contradictorios)
           (list "Advertencia" ":" "Tienes" "síntomas" "contradictorios" "verificados")
           (list "Tus" "síntomas" "parecen" "consistentes")))

      (flagArbol
       (let ((diags (get-arbol-diagnostico)))
         (if diags (append (list "Según" "el" "árbol" "de" "decisión" "tienes" ":") diags)
             (list "No" "cumples" "con" "todos" "los" "síntomas" "para" "un" "diagnóstico" "exacto"))))

      (flagDiagnosticoExclusivo
       (let ((res (get-diagnostico-exclusivo)))
         (if res (list "Diagnóstico" "exclusivo" ":" (first res) "debido" "al" "síntoma" "único" (second res))
             (list "No" "se" "ha" "encontrado" "un" "diagnóstico" "exclusivo" "con" "los" "síntomas" "proporcionados"))))

      (flagRiesgo
       (let ((diags (get-arbol-diagnostico)))
         (if (find arg-token diags :test #'string-equal)
             (let ((grav (second (assoc arg-token *gravedad-db* :test #'string-equal))))
               (list "El" "riesgo" "de" "la" arg-token "es" ":" grav))
             (list "No" "se" "detecta" "riesgo" "confirmado" "para" arg-token))))

      (flagTratamientoCombinado
       (let ((trats (get-tratamiento-combinado)))
         (if trats (append (list "Tratamientos" "sugeridos" ":") trats)
             (list "No" "hay" "tratamientos" "combinados" "aplicables"))))

      (flagRecomendacionSeveridad
       (let ((sev (get-severity arg-token)))
         (if sev
             (let* ((entry (assoc arg-token *recomendaciones-db* :test #'string-equal))
                    (rec (cdr (assoc sev (cdr entry) :test #'string-equal))))
               (list "Debido" "a" "severidad" sev ":" rec))
             (list "No" "puedo" "determinar" "la" "severidad" "para" "darte" "recomendación" "de" arg-token))))

      (flagDiagnosticarYTratar
       (let ((diags (get-arbol-diagnostico)))
         (if diags
             (let* ((d (first diags))
                    (t-text (second (assoc d *tratamientos-db* :test #'string-equal))))
               (list "Diagnóstico" ":" d "." "Tratamiento" ":" t-text))
             (list "No" "se" "pudo" "diagnosticar" "y" "tratar" "nada" "concretamente"))))

      (flagReporte
       (format t "--- REPORTE MÉDICO ---~%")
       (format t "Síntomas confirmados: ~a~%" *sintomas-usuario*)
       (format t "Enfermedades posibles (Probabilidad):~%")
       (dolist (entry *sintomas-db*)
         (let ((prob (get-probabilidad (car entry))))
           (when (> prob 0)
             (format t "  - ~a: ~,2f%~%" (car entry) prob))))
       (format t "Diagnóstico Final:~%")
       (let ((diags (get-arbol-diagnostico)))
         (if diags
             (dolist (d diags)
               (format t "  * Enfermedad: ~a~%" d)
               (let ((sev (get-severity d)))
                 (when sev (format t "    Severidad: ~a~%" sev)))
               (let ((tr (second (assoc d *tratamientos-db* :test #'string-equal))))
                 (when tr (format t "    Tratamiento: ~a~%" tr))))
             (format t "  No se cumplen condiciones para un diagnóstico definitivo.~%")))
       (format t "----------------------~%")
       (list "Reporte" "generado" "en" "consola" "."))

      (flagDiagnosticoProbabilidad
       (let ((best-enf nil) (best-prob 0))
         (dolist (entry *sintomas-db*)
           (let ((prob (get-probabilidad (car entry))))
             (when (> prob best-prob)
               (setf best-prob prob)
               (setf best-enf (car entry)))))
         (if (and best-enf (> best-prob 0))
             (list "El" "diagnóstico" "más" "probable" "es" best-enf "con" "un" (format nil "~,2f%" best-prob) "de" "probabilidad" ".")
             (list "No" "presento" "suficientes" "síntomas" "para" "un" "diagnóstico" "."))))

      (flagDiagnostico
       (format t "Para realizar el diagnóstico, responderé con preguntas.~%")
       (dolist (entry *sintomas-db*)
         (dolist (s (cdr entry))
           (unless (find s *sintomas-usuario* :test #'string-equal)
             (format t "¿Tienes ~a? " s)
             (finish-output)
             (let ((resp (read-line)))
               (when (or (string-equal resp "si") (string-equal resp "s") (string-equal resp "yes"))
                 (pushnew s *sintomas-usuario* :test #'string-equal))))))
       (handle-flag 'flagDiagnosticoProbabilidad nil nil))

      (flagPadre (let ((parents (get-parents arg-token)))
                   (if parents (append (list "El" "padre" "de" arg-token "es") parents)
                       (list "No" "sé" "quién" "es" "el" "padre" "de" arg-token))))
      (flagMadre (let ((parents (get-parents arg-token)))
                   (if parents (append (list "La" "madre" "de" arg-token "es") (intersection parents (mapcar #'car *madre-db*) :test #'string-equal))
                       (list "No" "sé" "quién" "es" "la" "madre" "de" arg-token))))
      (flagHijo (let ((children (get-children arg-token)))
                  (if children (append (list "Los" "hijos" "de" arg-token "son") children)
                      (list "No" "encontré" "hijos" "de" arg-token))))
      (flagHermano (let ((siblings (get-siblings arg-token)))
                     (if siblings (append (list "Los" "hermanos" "de" arg-token "son") siblings)
                         (list "No" "encontré" "hermanos" "de" arg-token))))
      (flagAbuelo (let ((gps (get-grandparents arg-token)))
                    (if gps (append (list "Los" "abuelos" "de" arg-token "son") gps)
                        (list "No" "encontré" "abuelos" "de" arg-token))))
      (flagTio (let ((uncles (get-uncles arg-token)))
                 (if uncles (append (list "Los" "tios" "de" arg-token "son") uncles)
                     (list "No" "encontré" "tios" "de" arg-token))))
      (flagSobrino (let ((neps (get-nephews arg-token)))
                     (if neps (append (list "Los" "sobrinos" "de" arg-token "son") neps)
                         (list "No" "encontré" "sobrinos" "de" arg-token))))
      (flagPrimo (let ((cousins (get-cousins arg-token)))
                   (if cousins (append (list "Los" "primos" "de" arg-token "son") cousins)
                       (list "No" "encontré" "primos" "de" arg-token))))
      
      (flagPersonajeDe (cond
                         ((string-equal arg-token "familia")
                          (append (list "Los" "miembros" "de" "la" "familia" "son" ":") *grupo-familia*))
                         ((string-equal arg-token "starwars")
                          (append (list "Los" "personajes" "de" arg-token "son" ":") *grupo-starwars*))
                         (t (list "No" "tengo" "información" "sobre" "el" "grupo" arg-token))))

      (flagContar (cond
                    ((string-equal arg-token "mujeres")
                     (let ((count (count-if (lambda (x) (find x *mujeres-db* :test #'string-equal)) *grupo-familia*)))
                       (list "Hay" (princ-to-string count) "mujeres" "en" "la" "familia")))
                    ((string-equal arg-token "hombres")
                     (let ((count (count-if (lambda (x) (find x *hombres-db* :test #'string-equal)) *grupo-familia*)))
                       (list "Hay" (princ-to-string count) "hombres" "en" "la" "familia")))
                    ((string-equal arg-token "padres")
                     (let ((padres (remove-duplicates (mapcar #'first *padre-db*) :test #'string-equal)))
                       (let ((count (count-if (lambda (p) (find p *grupo-familia* :test #'string-equal)) padres)))
                         (list "Hay" (princ-to-string count) "padres" "en" "la" "familia"))))
                    ((string-equal arg-token "madres")
                     (let ((madres (remove-duplicates (mapcar #'first *madre-db*) :test #'string-equal)))
                       (let ((count (count-if (lambda (m) (find m *grupo-familia* :test #'string-equal)) madres)))
                         (list "Hay" (princ-to-string count) "madres" "en" "la" "familia"))))
                    ((string-equal arg-token "hijos")
                     (let ((hijos '()))
                       (dolist (pair *padre-db*) (push (second pair) hijos))
                       (dolist (pair *madre-db*) (push (second pair) hijos))
                       (setf hijos (remove-duplicates hijos :test #'string-equal))
                       (let ((count (count-if (lambda (h) (find h *grupo-familia* :test #'string-equal)) hijos)))
                         (list "Hay" (princ-to-string count) "hijos" "en" "la" "familia"))))
                    ((string-equal arg-token "abuelos")
                     (let ((abuelos (remove-duplicates (mapcan (lambda (p) (get-grandparents p)) *grupo-familia*) :test #'string-equal)))
                       (let ((count (count-if (lambda (gp) (find gp *grupo-familia* :test #'string-equal)) abuelos)))
                         (list "Hay" (princ-to-string count) "abuelos" "(" "abuelos" "y" "abuelas" ")" "en" "la" "familia"))))
                    (t (list "No" "se" "como" "contar" arg-token "en" "la" "familia"))))

      (flagContarStarWars (cond
                            ((string-equal arg-token "mujeres")
                             (let ((count (count-if (lambda (x) (find x *mujeres-db* :test #'string-equal)) *grupo-starwars*)))
                               (list "Hay" (princ-to-string count) "mujeres" "en" "star" "wars")))
                            ((string-equal arg-token "hombres")
                             (let ((count (count-if (lambda (x) (find x *hombres-db* :test #'string-equal)) *grupo-starwars*)))
                               (list "Hay" (princ-to-string count) "hombres" "en" "star" "wars")))
                            ((string-equal arg-token "padres")
                             (let ((padres (remove-duplicates (mapcar #'first *padre-db*) :test #'string-equal)))
                               (let ((count (count-if (lambda (p) (find p *grupo-starwars* :test #'string-equal)) padres)))
                                 (list "Hay" (princ-to-string count) "padres" "en" "star" "wars"))))
                            ((string-equal arg-token "madres")
                             (let ((madres (remove-duplicates (mapcar #'first *madre-db*) :test #'string-equal)))
                               (let ((count (count-if (lambda (m) (find m *grupo-starwars* :test #'string-equal)) madres)))
                                 (list "Hay" (princ-to-string count) "madres" "en" "star" "wars"))))
                            ((string-equal arg-token "hijos")
                             (let ((hijos '()))
                               (dolist (pair *padre-db*) (push (second pair) hijos))
                               (dolist (pair *madre-db*) (push (second pair) hijos))
                               (setf hijos (remove-duplicates hijos :test #'string-equal))
                               (let ((count (count-if (lambda (h) (find h *grupo-starwars* :test #'string-equal)) hijos)))
                                 (list "Hay" (princ-to-string count) "hijos" "en" "star" "wars"))))
                            (t (list "No" "se" "como" "contar" arg-token "en" "star" "wars"))))

      (t (list "Please" "explain" "a" "little" "more" ".")))))

(defun respond-to (input)
  (let ((tpl (find-matching-template input)))
    (when tpl
      (let* ((resp (second tpl))
             (indices (third tpl)))
        (if (and (consp resp) (symbolp (first resp))
                 (string-equal (subseq (symbol-name (first resp)) 0 4) "FLAG"))
            (format t "~{~a~^ ~}~%" (handle-flag (first resp) indices input))
            (format t "~{~a~^ ~}~%" (build-response-from-resp resp indices input)))))))

(defun eliza-loop ()
  (format t "Hola, mi nombre es Eliza V2 (Common Lisp).~%")
  (format t "Por favor, ingresa tu consulta usando solo minúsculas y sin punto al final.~%")
  (loop
    (format t "~%> ")
    (let ((line (read-line *query-io* nil nil)))
      (when (null line) (return))
      (let ((tokens (clean-and-tokenize line)))
        (cond
          ((or (string= (first tokens) "adios")
               (string= (first tokens) "bye"))
           (format t "Adiós. Espero haberte ayudado.~%")
           (return))
          (t (respond-to tokens)))))))

;; Para ejecutar: (eliza-loop)