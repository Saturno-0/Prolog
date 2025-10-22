import flet as ft

def main(page):
    
    page.scroll = "auto"
    page.title = "Particiones de un número"
    
    n = ft.TextField(label="Número a particionar" )
    maximo = n - 1
    page.add(n)
    
    page.add(ft.ElevatedButton("Calcular particiones", on_click=lambda e: particiones(int(n.value), maximo)))
    
    def particiones(n, maximo):
        
        if n == 0:
            # caso base: devolvemos una lista vacía que representa una partición
            yield []
        else:
            for k in range(min(n, maximo), 0, -1):
                for resto in particiones(n - k, k):
                    yield []

    for p in particiones(n, maximo):
        page.add(ft.TextField(p))

ft.app(main)