| sistema turnos canchas |
sistema := SistemaAlquiler new initialize.
turnos := 	GeneradorDeDatos generarTurnos.
canchas := GeneradorDeDatos generarCanchas: turnos.

canchas inspect.
