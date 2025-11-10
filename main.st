|sistema canchas acciones salir |

"Asignacion de datos iniciales"
sistema := SistemaAlquilerDeCanchas new initialize.
GeneradorDeDatos new initialize.
canchas := GeneradorDeDatos generarCanchasConTurnos: 3 cantidadDeTurnosPorDia: 5.
sistema canchas: canchas.
sistema metodosPago: (GeneradorDeDatos generarMetodosPago).
sistema registrarCliente: (Cliente dni: '1' nombre: 'TEST' telefono: '1234567890' ). 


"Acciones post eleccion de opcion de menu"
acciones := Dictionary new.
	acciones at: '0' put: [ "Finalizar programa: "
		salir := true.
		Transcript cr; show: 'Saliendo... '; cr.
	].
	acciones at: '1' put: ["Registrar cliente: "
		Transcript show: '----- REGISTRAR CLIENTE -----'; cr.
		sistema registrarCliente.
	].
	acciones at: '2' put: ["Registrar reserva: "
		Transcript show: '----- REGISTRAR RESERVA -----'; cr.
		sistema registrarReserva.
	].
	acciones at: '3' put: ["Cancelar reserva: "
		Transcript show: '----- CANCELAR RESERVA -----'; cr.
		sistema cancelarReserva.
	].
	acciones at: '4' put: ["Listar todos los clientes: "
		Transcript show: '----- LISTA DE CLIENTES -----'; cr.
		Transcript cr.
		(sistema clientes)
		do: [:c | 
			Transcript show: 'Nombre: ', c nombre; cr.
			Transcript show: 'DNI: ', c dni; cr.
			Transcript show: 'Teléfono: ', c telefono; cr; cr.
		].
		MessageBox notify:  'Presione ENTER para continuar.'.
	].
	acciones at: '5' put: ["Realizar Pago de Reserva: "	
		Transcript show: '------- REALIZAR PAGO -------'; cr.
		sistema procesarPagoDesdeCero.
	].
	acciones at: '6' put: ["Ver Reservas de un Cliente: "
		Transcript show: '------- RESERVAS DEL CLIENTE -------'; cr.
		sistema verReservasDeCliente.
	].

"Menu"
salir := false.
[salir] whileFalse: [
	Transcript clear.
	Transcript show: '===== BIENVENIDO ====='; cr.
	Transcript show: '------------ MENU ------------'; cr.
	Transcript show: '1. Registrar Nuevo Cliente'; cr.
	Transcript show: '2. Registrar Reserva'; cr.
	Transcript show: '3. Cancelar Reserva'; cr.
	Transcript show: '4. Listar Todos Los Clientes'; cr.
	Transcript show: '5. Realizar Pago de Reserva'; cr. 
	Transcript show: '6. Ver Reservas de un Cliente'; cr. 
	Transcript show: '0. Salir'; cr.
	Transcript show: '---------------------------------'; cr.
	opcion := Prompter prompt: 'Seleccione una opcion: '.	
	Transcript show: 'Opción ingresada: ', opcion; cr.
	
		
	"Ejecución de accion asociada a opcion elegida"
	bloqueAccion := acciones at: opcion 
						  ifAbsent: [ 
							[ MessageBox notify: 'Opción ingresada no válida. Intente de nuevo.']
						   ]. 
	bloqueAccion value.
].

sistema inspect.
