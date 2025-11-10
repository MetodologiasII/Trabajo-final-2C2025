| package |
package := Package name: 'SistemaAlquilerCancha'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Cancha;
	add: #Cliente;
	add: #GeneradorDeDatos;
	add: #MetodoPago;
	add: #Pago;
	add: #Reserva;
	add: #SistemaAlquilerDeCanchas;
	add: #Turno;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Cancha
	instanceVariableNames: 'id tipo precioHora turnos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Cliente
	instanceVariableNames: 'dni nombre telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #GeneradorDeDatos
	instanceVariableNames: ''
	classVariableNames: 'ContadorIds'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #MetodoPago
	instanceVariableNames: 'id nombre'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Pago
	instanceVariableNames: 'id fechaHora idMetodoPago idReserva'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Reserva
	instanceVariableNames: 'id dniCliente canchaId turnoId estado estadoPago'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #SistemaAlquilerDeCanchas
	instanceVariableNames: 'nombre clientes canchas reservas pagos metodosPago'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Turno
	instanceVariableNames: 'id fechaHoraInicio fechaHoraFin estado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

Cancha guid: (GUID fromString: '{a9487327-99ac-464c-a1ba-fb92651f0fb5}')!

Cancha comment: ''!

!Cancha categoriesForClass!Kernel-Objects! !

!Cancha methodsFor!

buscarTurno: unTurnoId
^turnos detect: [:t | t id = unTurnoId ] ifNone: [^nil].!

id
^ id.!

id: unId
id := unId!

precioHora
^precioHora!

precioHora: unPrecioHora
precioHora :=unPrecioHora.!

tipo 
^ tipo!

tipo: unTipo
tipo := unTipo.!

turnos 
^turnos.!

turnos: unaListaDeTurnos
turnos := unaListaDeTurnos!

turnosDisponibles
^turnos select: [:t | t estado = 'DISPONIBLE']! !

!Cancha categoriesForMethods!
buscarTurno:!public! !
id!public! !
id:!public! !
precioHora!public! !
precioHora:!public! !
tipo!public! !
tipo:!public! !
turnos!public! !
turnos:!public! !
turnosDisponibles!public! !
!

!Cancha class methodsFor!

id: unId tipo: unTipo precioHora: unPrecioHora turnos: unaListaDeTurnos
^self new id: unId; tipo: unTipo; precioHora: unPrecioHora; turnos: unaListaDeTurnos.! !

!Cancha class categoriesForMethods!
id:tipo:precioHora:turnos:!public! !
!

Cliente guid: (GUID fromString: '{b054204d-f2da-4ae9-9cb1-09eddd54334a}')!

Cliente comment: ''!

!Cliente categoriesForClass!Kernel-Objects! !

!Cliente methodsFor!

dni
^dni.!

dni: unDni
dni := unDni!

nombre
^nombre.!

nombre: unNombre
nombre := unNombre .!

telefono
^telefono .!

telefono: unTelefono
telefono := unTelefono .! !

!Cliente categoriesForMethods!
dni!public! !
dni:!public! !
nombre!public! !
nombre:!public! !
telefono!public! !
telefono:!public! !
!

!Cliente class methodsFor!

dni: unDni nombre: unNombre telefono: unTelefono
^self new dni: unDni;  nombre: unNombre; telefono: unTelefono.
	!

validarFormatoDni: unDni

"Validación de cadena vacía"
(unDni isEmpty)
ifTrue: [
    MessageBox notify: 'Error: El DNI no puede estar vacío.'.
    ^ false
].

"Validación de formato numérico"
(unDni allSatisfy: [:char | char isDigit])
ifFalse: [
    MessageBox notify: 'Error: El DNI debe contener solo números.'.
    ^ false
].
^ true! !

!Cliente class categoriesForMethods!
dni:nombre:telefono:!public! !
validarFormatoDni:!public! !
!

GeneradorDeDatos guid: (GUID fromString: '{5ef58c88-9df1-4e2d-bb76-5b4fb37a4ccd}')!

GeneradorDeDatos comment: ''!

!GeneradorDeDatos categoriesForClass!Kernel-Objects! !

!GeneradorDeDatos methodsFor!

initialize
super initialize.

ContadorIds := Dictionary new.
ContadorIds at: 'canchas' put: 1.
ContadorIds at: 'turnos' put: 1.
ContadorIds at: 'reservas' put: 1.
ContadorIds at: 'pagos' put: 1.
ContadorIds at: 'metodosPago' put: 1.! !

!GeneradorDeDatos categoriesForMethods!
initialize!public! !
!

!GeneradorDeDatos class methodsFor!

asignarCanchaId
|id|
id:= ContadorIds at: 'canchas'.
ContadorIds at: 'canchas' put: id + 1.
^id!

asignarMetodoPagoId
|id|
id:= ContadorIds at: 'metodosPago'.
ContadorIds at: 'metodosPago' put: id + 1.
^id!

asignarPagoId
|id|
id:= ContadorIds at: 'pagos'.
ContadorIds at: 'pagos' put: id + 1.
^id!

asignarReservaId
|id|
id:= ContadorIds at: 'reservas'.
ContadorIds at: 'reservas' put: id + 1.
^id!

asignarTurnoId
|id|
id:= ContadorIds at: 'turnos'.
ContadorIds at: 'turnos' put: id + 1.
^id!

generarCanchasConTurnos: cantidadCanchas cantidadDeTurnosPorDia: cantidadTurnos
| canchas tiposYprecios tipoPrecio tipos|
canchas := OrderedCollection new.

tiposYprecios := Dictionary new.
tiposYprecios at: 'Fútbol 5' put: 1000.
tiposYprecios at: 'Fútbol 7' put: 1500.
tiposYprecios at: 'Fútbol 9' put: 2000. 



"Obtener la lista de tipos para iterar sobre ellos"
tipos := tiposYprecios keys asArray. 

1 to: cantidadCanchas do: [:i |
	| tipo canchaId |
	
	"Rotar entre los tipos F5, F7, F9 para no crear canchas idénticas"
	tipo := tipos at: ( (i - 1) rem: tipos size) + 1. 
	tipoPrecio := tiposYprecios at: tipo.

	canchaId := self asignarCanchaId . 
    
	canchas add: ( 
        Cancha 
            id: canchaId 
            tipo: tipo 
            precioHora: tipoPrecio 
            turnos: (self generarTurnos: cantidadTurnos)
    ).
].

^ canchas!

generarCanchasSinTurnos: cantidad 
| canchas tiposYprecios tipoPrecio tipos|
canchas := OrderedCollection new.

tiposYprecios := Dictionary new.
tiposYprecios at: 'Fútbol 5' put: 1000.
tiposYprecios at: 'Fútbol 7' put: 1500.
tiposYprecios at: 'Fútbol 9' put: 2000. 

"Obtener la lista de tipos para iterar sobre ellos"
tipos := tiposYprecios keys asArray. 

1 to: cantidad do: [:i |
	| tipo canchaId |
	
	"Rotar entre los tipos F5, F7, F9 para no crear canchas idénticas"
	tipo := tipos at: ( (i - 1) rem: tipos size) + 1. 
	tipoPrecio := tiposYprecios at: tipo.

	canchaId := self asignarCanchaId . 
    
	canchas add: ( 
        Cancha 
            id: canchaId 
            tipo: tipo 
            precioHora: tipoPrecio 
            turnos: nil
    ).
].

^ canchas!

generarMetodosPago

| metodosPago | 
metodosPago := OrderedCollection new.
metodosPago add: (MetodoPago id: (self  asignarMetodoPagoId) nombre: 'Efectivo').
metodosPago add: (MetodoPago id: (self  asignarMetodoPagoId) nombre: 'Débito').
metodosPago add: (MetodoPago id: (self  asignarMetodoPagoId) nombre: 'Crédito').

^metodosPago.!

generarTurnos: cantidadPorDia
"Genera turnos con estado 'DISPONIBLE' para los proximos 3 dias"
| listaDeTurnos fechaHoraBase fechaHoraTurno turno |

"Validacion de cantidad ingresada"
(cantidadPorDia > 10)
ifTrue: [
	MessageBox error: 'Solo se pueden generar 10 turnos por dia como maximo'.
	^self
].


listaDeTurnos := OrderedCollection new.

"Seteo de FechaHoraBase para el dia de Hoy a las 12:00 hs"
fechaHoraBase := DateAndTime today + (Duration hours: 11).

1 to: 3 do: [ :i |
	"La variable i almacena el contador de las fechas"
	"Se toma como fecha y hora inicial el dia de mañana a las 12:00 hs"
	fechaHoraTurno := fechaHoraBase + (Duration days: i).
	
	1 to: cantidadPorDia do: [ :j |
		"La variable j almacena el contador de las horas (El segundo bucle se repite 10 veces por cada iteracion del bucle anterior)"
		fechaHoraTurno := fechaHoraTurno + (Duration hours: 1).
		turno := Turno id: (self asignarTurnoId ) fechaHoraInicio: fechaHoraTurno fechaHoraFin: (fechaHoraTurno  + (Duration minutes: 59) ) estado: 'DISPONIBLE'.
		listaDeTurnos add: turno.
	]	
].

^listaDeTurnos! !

!GeneradorDeDatos class categoriesForMethods!
asignarCanchaId!public! !
asignarMetodoPagoId!public! !
asignarPagoId!public! !
asignarReservaId!public! !
asignarTurnoId!public! !
generarCanchasConTurnos:cantidadDeTurnosPorDia:!public! !
generarCanchasSinTurnos:!public! !
generarMetodosPago!public! !
generarTurnos:!public! !
!

MetodoPago guid: (GUID fromString: '{f40069d8-c770-4bb3-be17-292fdf0daf45}')!

MetodoPago comment: ''!

!MetodoPago categoriesForClass!Kernel-Objects! !

!MetodoPago methodsFor!

id 
^ id!

id: unId
id := unId!

nombre 
^nombre!

nombre: unNombre
nombre := unNombre! !

!MetodoPago categoriesForMethods!
id!public! !
id:!public! !
nombre!public! !
nombre:!public! !
!

!MetodoPago class methodsFor!

id: unId nombre: unNombre
^self new id: unId; nombre: unNombre.! !

!MetodoPago class categoriesForMethods!
id:nombre:!public! !
!

Pago guid: (GUID fromString: '{66154e6b-c824-439e-ba94-9d1109bc9724}')!

Pago comment: ''!

!Pago categoriesForClass!Kernel-Objects! !

!Pago methodsFor!

fechaHora
^ fechaHora!

fechaHora: unaFechaHora
fechaHora := unaFechaHora!

id
^id!

id: unId
id:= unId!

idMetodoPago
^ idMetodoPago.
!

idMetodoPago: unIdMetodoPago
idMetodoPago := unIdMetodoPago!

idReserva
^idReserva!

idReserva: unIdReserva
idReserva:= unIdReserva! !

!Pago categoriesForMethods!
fechaHora!public! !
fechaHora:!public! !
id!public! !
id:!public! !
idMetodoPago!public! !
idMetodoPago:!public! !
idReserva!public! !
idReserva:!public! !
!

!Pago class methodsFor!

id: unId fechaHora: unaFechaHora idMetodoPago: unIdMetodoPago idReserva: unIdReserva
^self new id: unId; fechaHora: unaFechaHora; idMetodoPago: unIdMetodoPago; idReserva: unIdReserva.! !

!Pago class categoriesForMethods!
id:fechaHora:idMetodoPago:idReserva:!public! !
!

Reserva guid: (GUID fromString: '{085e79e1-9a24-4c9a-82bc-c48d59108fe2}')!

Reserva comment: ''!

!Reserva categoriesForClass!Kernel-Objects! !

!Reserva methodsFor!

canchaId
^canchaId!

canchaId: unCanchaId
canchaId:= unCanchaId!

dniCliente
^dniCliente!

dniCliente: unDniCliente
dniCliente := unDniCliente!

estado
^estado!

estado: unEstado
estado := unEstado!

estadoPago
^estadoPago!

estadoPago: unEstadoPago
estadoPago := unEstadoPago!

id 
^id!

id: unId
id:= unId.!

turnoId
^ turnoId!

turnoId: unTurnoId
turnoId := unTurnoId! !

!Reserva categoriesForMethods!
canchaId!public! !
canchaId:!public! !
dniCliente!public! !
dniCliente:!public! !
estado!public! !
estado:!public! !
estadoPago!public! !
estadoPago:!public! !
id!public! !
id:!public! !
turnoId!public! !
turnoId:!public! !
!

!Reserva class methodsFor!

id: unId dniCliente: unDniCliente canchaId: unCanchaId turnoId: unTurnoId estado: unEstado
^self new id: unId; dniCliente: unDniCliente; canchaId: unCanchaId; turnoId: unTurnoId ;estado: unEstado.!

id: unId dniCliente: unDniCliente canchaId: unCanchaId turnoId: unTurnoId estado: unEstado estadoPago: unEstadoPago
^self new id: unId; dniCliente: unDniCliente; canchaId: unCanchaId; turnoId: unTurnoId ;estado: unEstado; estadoPago: unEstadoPago .! !

!Reserva class categoriesForMethods!
id:dniCliente:canchaId:turnoId:estado:!public! !
id:dniCliente:canchaId:turnoId:estado:estadoPago:!public! !
!

SistemaAlquilerDeCanchas guid: (GUID fromString: '{6f48e06c-e524-4694-b3bc-de4c43bbde2a}')!

SistemaAlquilerDeCanchas comment: ''!

!SistemaAlquilerDeCanchas categoriesForClass!Kernel-Objects! !

!SistemaAlquilerDeCanchas methodsFor!

buscarCancha: canchaId
^canchas detect: [:c | c id = canchaId ] ifNone: [^nil].!

buscarCliente: dniCliente
^clientes detect: [:c | c dni = dniCliente ] ifNone: [^nil].!

buscarMetodoPago: idMetodoPago
^metodosPago detect: [:m | m id = idMetodoPago ] ifNone: [^nil].!

buscarReserva: reservaId

^reservas detect: [:r | r id = reservaId ] ifNone: [nil].!

cancelarReserva

| dniCliente id reserva cancha turno confirmar |

dniCliente := Prompter prompt: 'Ingrese el dni del cliente.'.
(self clienteExiste: dniCliente)
ifFalse: [
	MessageBox notify: 'No se encuentra registrado ningun cliente con DNI: ', dniCliente.
	^self.
].

"Impresión de reservas del cliente"
Transcript show: '----- RESERVAS DEL CLIENTE { ', dniCliente, ' } -----'; cr.
((self reservasNoCanceladasDeCliente: dniCliente) size = 0)
ifTrue: [
	Transcript show: 'El cliente no posee reservas no canceladas.'; cr.
	MessageBox notify: 'El cliente no posee reservas no canceladas.'.
	^self.
].
 

(self reservasNoCanceladasDeCliente: dniCliente) 
	do: [:r |
		cancha := self buscarCancha: r canchaId.
		turno := cancha buscarTurno: r turnoId.
		Transcript show: 'ID: ', r id printString ; cr. 
		Transcript show: 'Cancha: ', cancha  tipo; cr.
		Transcript show: 'Fecha: ', turno fechaHoraInicio asDate printString ; cr. 
		Transcript show: 'Hora: ', turno  fechaHoraInicio  asTime printString ; cr.
		Transcript cr.
	].

[id:= (Prompter prompt: 'Ingrese el ID de la reserva a cancelar.') asNumber]
on: Error 
do: [:ex | 
	MessageBox notify:'El ID ingresado debe ser un número entero.' .
	^self.
].

reserva := self buscarReserva: id.
"Validación de existencia de reserva"
(reserva isNil)
ifTrue: [
	MessageBox notify: 'No se encontró ningúna reserva con el ID: ', id printString.
	^self.
].

"Validación de reserva perteneciente al cliente"
(reserva dniCliente ~= dniCliente )
ifTrue: [
	MessageBox notify: 'No se encontró ningúna reserva con el ID: ', id printString, ' que pertenezca al cliente con dni ', dniCliente.
	^self.
].

"En este punto no se hacen validaciones de existencia de cancha y turno porque se validan previo a la creación de la reserva, es decir, si la reserva existe, entonces sus datos son validos."
cancha := self buscarCancha: reserva canchaId.
turno := cancha buscarTurno: reserva turnoId.

confirmar := MessageBox confirm: 'Confirmar cancelación de reserva?'.
(confirmar)
ifFalse: [
	MessageBox notify: 'Operación cancelada.'.
	^self.
].

turno estado: 'DISPONIBLE'.
reserva estado: 'CANCELADO'.
MessageBox notify: 'Reserva cancelada exitosamente.'
!

canchas
^canchas!

canchas: unaListaDeCanchas
canchas := unaListaDeCanchas!

clienteExiste: dniCliente

| existe |
existe := clientes anySatisfy: [:cliente | cliente dni = dniCliente ].
^existe.!

clientes
^clientes!

initialize
super initialize.

nombre := 'Sistema de Alquiler de Canchas'.
clientes := OrderedCollection new.
canchas := OrderedCollection new.
reservas := OrderedCollection new.
pagos := OrderedCollection new.
metodosPago := OrderedCollection new.!

metodosPago
^metodosPago!

metodosPago: unaListaDeMetodosDePago
metodosPago:= unaListaDeMetodosDePago!

nombre
^nombre!

nombre: unNombre
nombre := unNombre!

pagos
^pagos!

procesarPago: reservaId
| reserva cancha turno idMetodoPago metodoPago pago confirmar |
"Validacion y busqueda de reserva"
reserva := reservas detect: [:r | r id = reservaId ] 
				ifNone: [ 
					MessageBox notify: 'El ID de la reserva a pagar ingresado no corresponde a ninguna reserva almacenada.'. 
					^self
				].

cancha := self buscarCancha: (reserva canchaId).
turno := cancha buscarTurno: reserva turnoId.

Transcript show: '-------PAGO DE RESERVA-------';cr.
Transcript show: 'Datos de la reserva a pagar: ';cr.
Transcript show: 'ID de reserva: ', reserva id printString ;cr.
Transcript show: 'Fecha: ', turno  fechaHoraInicio asDate  printString ;cr.
Transcript show: 'Hora inicio: ', turno  fechaHoraInicio asTime  printString ;cr.
Transcript show: 'Hora fin: ', turno  fechaHoraFin asTime printString ;cr.
Transcript show: 'Monto total: ', cancha precioHora printString ;cr.

Transcript show: '---- MÉTODOS DE PAGO ----';cr.
metodosPago do: [:m | 
		Transcript show: 'ID: ',m id printString , ' | ', m nombre; cr.
].
[idMetodoPago:= (Prompter prompt: 'Ingrese el ID del metodo de pago a utilizar.') asNumber]
	on: Error 
	do: [:ex | MessageBox notify: 'El ID ingresado debe ser un número.'. ^self ].

metodoPago := self buscarMetodoPago: idMetodoPago.
(metodoPago isNil) ifTrue: [
	MessageBox notify: 'La opción ingresada no pertenece a ningun metodo de pago. La operación fue cancelada.'.
	^self.

].

Transcript show: '---- DETALLES DEL PAGO ----';cr.
Transcript show: 'Total a pagar: ', cancha precioHora printString ;cr.
Transcript show: 'Método de pago: ', metodoPago nombre;cr.
Transcript cr; show: 'Confirmar? ';cr.

confirmar := MessageBox confirm: 'Confirmar pago de $', cancha precioHora printString,' con el metodo de pago en ', metodoPago nombre ,'?...'.
(confirmar ) ifFalse: [
	MessageBox  notify: 'Pago cancelado.'. 
	^self 
].

pago := Pago id: (GeneradorDeDatos asignarPagoId ) fechaHora: (DateAndTime now ) idMetodoPago: idMetodoPago idReserva: reservaId.
reserva estadoPago: 'COMPLETO'.
pagos add: pago.
MessageBox notify: 'Pago realizado exitosamente.'!

procesarPagoDesdeCero

| dniCliente cancha turno idReserva reserva |

dniCliente := Prompter prompt: 'Ingrese el DNI del cliente'.
(self clienteExiste: dniCliente)
ifFalse: [
	MessageBox notify: 'El cliente con DNI ', dniCliente, ' no se encuentra registrado'.
	^self.
].

"Impresion de reservas no pagas del cliente"
(self reservasNoCanceladasDeCliente: dniCliente )
do: [:r | 
	(r estadoPago = 'INCOMPLETO')
	ifTrue: [
		cancha := self buscarCancha: r canchaId.
		turno := cancha buscarTurno: r turnoId.
		Transcript show: 'ID: ', r id printString ; cr. 
		Transcript show: 'Cancha: ', cancha  tipo; cr.
		Transcript show: 'Fecha: ', turno fechaHoraInicio asDate printString ; cr. 
		Transcript show: 'Hora: ', turno  fechaHoraInicio  asTime printString ; cr.
		Transcript show: 'Estado de pago: ', r estadoPago; cr.
		Transcript cr.
	].
].

[idReserva := (Prompter  prompt: 'Ingrese el id de la reserva a pagar') asNumber ]
on: Error 
do: [:ex | 
	MessageBox notify: 'El ID ingresado debe ser un número.'.	
	^self
].

reserva := self buscarReserva: idReserva.
(reserva isNil or: [reserva  dniCliente ~= dniCliente] )
ifTrue: [
	MessageBox notify: 'El ID ingresado no pertenece a ninguna de sus reservas.'.
	^self
].

(reserva estadoPago = 'COMPLETO')
ifTrue: [
	MessageBox notify: 'El ID ingresado pertenece a una de sus reservas que ya fueron pagadas.'.
	^self.
].


self procesarPago: reserva id.

!

registrarCliente
	| dniCliente esValido cliente nombreCliente telefonoCliente confirmar|
	dniCliente := (Prompter prompt: 'Ingrese el DNI del cliente: ').

	"Si el dni solo posee numero y no letras, entonces continua pidiendo datos, de lo contrario, cancela la operación."
	esValido := Cliente validarFormatoDni: dniCliente.
	(esValido) 
	ifFalse: [
		^self.
	].
	"Validación de cliente ya registrado: "
	(self clienteExiste: dniCliente )
	ifTrue: [
		MessageBox notify: 'El DNI ingresado ya se encuentra registrado.'.
		^self.
	].

	"Solicitud de datos restantes del cliente: "
	nombreCliente := Prompter prompt: 'Ingrese el nombre del cliente: '.
	telefonoCliente := Prompter prompt: 'Ingrese el telefono del cliente: '.
	
	cliente := Cliente dni: dniCliente nombre: nombreCliente telefono: telefonoCliente.
	
	Transcript show: 'Datos ingresados: '; cr.
	Transcript show: '- DNI: ', dniCliente ; cr.
	Transcript show: '- Nombre: ', nombreCliente ; cr.
	Transcript show: '- Teléfono ', telefonoCliente ; cr.
	Transcript show: 'Confirmar registro?'; cr.

	confirmar := MessageBox confirm: 'Confirmar registro del cliente con DNI: ', dniCliente printString.
	(confirmar)
	ifFalse: [
		MessageBox notify: 'Registro cancelado.'.
		^self.
	].
	clientes  add: cliente.
	MessageBox notify: 'Cliente registrado exitosamente...'.
!

registrarCliente: unCliente
clientes add: unCliente!

registrarReserva
	| dniCliente esValido cliente idCancha cancha idTurno turno reserva confirmar |
	dniCliente := (Prompter prompt: 'Ingrese el DNI del cliente: ').

	"Si el dni solo posee numero y no letras, entonces continua pidiendo datos, de lo contrario, cancela la operación."
	esValido := Cliente validarFormatoDni: dniCliente.
	(esValido) 
	ifFalse: [
		^self.
	].
	"Validación de cliente registrado: "
	cliente := self buscarCliente: dniCliente.
	(cliente isNil)
	ifTrue: [
		MessageBox notify: 'El DNI ingresado no se encuentra registrado.'.
		^self.
	].

	"Mostrar canchas disponibles"
	Transcript show: '================================='; cr.
	Transcript show: '||                  CANCHAS DISPONIBLES                  ||'; cr.
	Transcript show: '================================='; cr.

	canchas do: [:c |
	    Transcript 
		show: 'ID: ', (c id printString); cr;  
		show: 'Tipo: ', c tipo; cr; 
		show: 'Precio/Hr: $', (c precioHora printString) ; cr ; cr.
	].
	Transcript show: '---------------------------------------------'; cr.
	
	"Solicitud y validacion del id de la cancha elegida. "
	[idCancha := (Prompter prompt: 'Introduce el ID de la cancha elegida para ver los horarios disponibles. ') asNumber ]
	on: Error 
	do: [:ex | 
		MessageBox notify: 'El ID ingresado debe ser un número entero.'.
		^self.		
	].	
	cancha := canchas detect: [:c | c id = idCancha ] 
					ifNone: [
						MessageBox notify: 'El ID ingresado no pertenece a ninguna de las canchas mostradas.'.
						^self.
					].
	
	"Impresion de turnos disponibles de la cancha elegida"
	Transcript show: '------------------------------------------------------------------------------------------'; cr.
	Transcript show: '======= TURNOS DISPONIBLES DE LA CANCHA  ', idCancha printString, ' ======='; cr; cr.
	cancha turnosDisponibles do: [:t | 
		Transcript show: 'ID: ', t id printString, ' | '; 
				show: 'Fecha: ', t fechaHoraInicio asDate printString, ' | '; 
				show: 'Hora ', t fechaHoraInicio asTime printString, ' | '; cr. 
	].
	Transcript show: '------------------------------------------------------------------------------------------'; cr.
	
	"Validación de formato de id ingresado"
	[idTurno := (Prompter prompt: 'Introduce el ID del turno a reservar. ') asNumber ]
	on: Error 
	do: [:ex | 
		MessageBox notify: 'El ID ingresado debe ser un número entero.'.
		^self.		
	].
	
	"Validación de id ingresado correspondiente a algun turno mostrados."
	(cancha turnosDisponibles anySatisfy: [:t | t id = idTurno])
	ifFalse: [
		MessageBox notify: 'El ID ingresado no pertenece a ningun turno ofrecido.'.
		^self.
	].
	
	
	
	
	turno := cancha turnos detect: [:t | t id = idTurno ] 
						ifNone: [
							MessageBox notify: 'El ID ingresado no pertenece a ningun turno mostrado. La operación se canceló.'.
							^self.
						].
	confirmar := MessageBox confirm: 'Confirmar reserva del turno para el dia ', turno fechaHoraInicio asDate printString, ' a las ', turno fechaHoraInicio asTime printString. 
	(confirmar)
	ifFalse: [
		MessageBox notify: 'Operación cancelada.'.
		^self.
	].

	turno estado: 'OCUPADO'.
	reserva := Reserva id: (GeneradorDeDatos asignarReservaId) dniCliente: dniCliente canchaId: idCancha turnoId: (turno id) estado: 'RESERVADA' estadoPago: 'INCOMPLETO'.
	reservas add: reserva.
	MessageBox notify: 'Reserva realizada exitosamente.'.

	confirmar := MessageBox confirm: 'Desea continuar con el pago?'.
	(confirmar )
	ifFalse: [
		^self.
	].
	self procesarPago: (reserva id).
	^self.
	
	


!

reservas
^reservas!

reservasCanceladasDeCliente: dniCliente
| reservasCliente reservasFiltradas|

reservasCliente := reservas select: [:r | r dniCliente  = dniCliente ].
reservasFiltradas := reservasCliente select: [:r | r estado = 'CANCELADO' ].

^reservasFiltradas.!

reservasDeCliente: dniCliente
^ reservas select: [:r | r dniCliente  = dniCliente ].
!

reservasNoCanceladasDeCliente: dniCliente
| reservasCliente reservasFiltradas|

reservasCliente := reservas select: [:r | r dniCliente  = dniCliente ].
reservasFiltradas := reservasCliente reject: [:r | r estado = 'CANCELADO' ].

^reservasFiltradas.!

verReservasDeCliente

|dniCliente cancha turno reservasCliente|

dniCliente := Prompter prompt: 'Ingrese el DNI del cliente.'.

"Validación de existencia del cliente"
(self  clienteExiste: dniCliente )
ifFalse: [
	MessageBox notify: 'El DNI ingresado no corresponde a ningun cliente registrado.'.
	   ^self.
].

reservasCliente := self reservasDeCliente: dniCliente.
(reservasCliente size = 0)
ifTrue: [
	Transcript show: 'El cliente con DNI ', dniCliente ,' no tiene reservas hasta el momento.'.
]
ifFalse: [
	(reservasCliente)
	do: [:reserva | 
		cancha := self buscarCancha: reserva canchaId.
		turno := cancha buscarTurno: reserva turnoId.
		Transcript show: 'ID: ', reserva id printString ; cr. 
		Transcript show: 'Cancha: ', cancha  tipo; cr.
		Transcript show: 'Fecha: ', turno fechaHoraInicio asDate printString ; cr. 
		Transcript show: 'Hora: ', turno  fechaHoraInicio  asTime printString ; cr.
		Transcript show: 'Estado de Pago: ', reserva estadoPago ; cr.
		Transcript show: 'Estado de reserva: ', reserva estado ; cr.
		Transcript cr.
	].
].


MessageBox notify: 'Presione ENTER para continuar.'.! !

!SistemaAlquilerDeCanchas categoriesForMethods!
buscarCancha:!public! !
buscarCliente:!public! !
buscarMetodoPago:!public! !
buscarReserva:!public! !
cancelarReserva!public! !
canchas!public! !
canchas:!public! !
clienteExiste:!public! !
clientes!public! !
initialize!public! !
metodosPago!public! !
metodosPago:!public! !
nombre!public! !
nombre:!public! !
pagos!public! !
procesarPago:!public! !
procesarPagoDesdeCero!public! !
registrarCliente!public! !
registrarCliente:!public! !
registrarReserva!public! !
reservas!public! !
reservasCanceladasDeCliente:!public! !
reservasDeCliente:!public! !
reservasNoCanceladasDeCliente:!public! !
verReservasDeCliente!public! !
!

Turno guid: (GUID fromString: '{a0692e47-2741-4137-a7e4-7b1b3733acb5}')!

Turno comment: ''!

!Turno categoriesForClass!Kernel-Objects! !

!Turno methodsFor!

estado
^estado.!

estado: unEstado
estado:= unEstado.!

fechaHoraFin
^fechaHoraFin.!

fechaHoraFin: unaFechaHoraFin
fechaHoraFin:= unaFechaHoraFin .!

fechaHoraInicio
^fechaHoraInicio.!

fechaHoraInicio: unaFechaHoraInicio
fechaHoraInicio:= unaFechaHoraInicio.!

id
^id.!

id: unId
id:= unId! !

!Turno categoriesForMethods!
estado!public! !
estado:!public! !
fechaHoraFin!public! !
fechaHoraFin:!public! !
fechaHoraInicio!public! !
fechaHoraInicio:!public! !
id!public! !
id:!public! !
!

!Turno class methodsFor!

id: unId fechaHoraInicio: unaFechaHoraInicio fechaHoraFin: unaFechaHoraFin estado: unEstado
^self new id: unId; fechaHoraInicio: unaFechaHoraInicio; fechaHoraFin: unaFechaHoraFin; estado: unEstado.! !

!Turno class categoriesForMethods!
id:fechaHoraInicio:fechaHoraFin:estado:!public! !
!

"Binary Globals"!

