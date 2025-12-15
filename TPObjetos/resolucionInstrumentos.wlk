/*Instrumentos*/

/*Guitarra*/
object guitarraFender {
  var color = "negro"
  
  const suenaBien = true

  method estaAfinada() = suenaBien 
  
  method costo() {
    if(color == "negro"){
      return 15
	  }else{
      return 10
	  } 
  }
  
  method esValiosa() = true 
  
  method color(nuevoColor) {
    color = nuevoColor
	  }
  }

/*Trompeta*/ 

object trompetaJupiter {

  var temperaturaAmbiente = 21 
  
  var tieneSordina = true

  method temperaturaAmbiente(nuevaTemperatura) {
    temperaturaAmbiente = nuevaTemperatura
  }


 method estaAfinada() = temperaturaAmbiente.between(20, 25)

  method afinar(grados) {
    if(!self.estaAfinada()){
      temperaturaAmbiente += grados
    } 
  }

  method tieneSordina(valor) {
    tieneSordina = valor
  }

  method costo(){
    if(tieneSordina){
      return 35
    }else{
      return 30
    }
  }

  method esCara() = self.costo() > 20

  method esValiosa() = false
}

/*Piano*/
object pianoBechstein {
  
  var largoDeHabitacion = 5
  var anchoDeHabitacion = 5
  
  var ultimaRevision = new Date()

  method habitacion() = largoDeHabitacion * anchoDeHabitacion

  method moverDeHabitacion(nuevoLargo, nuevoAncho) {
    largoDeHabitacion = nuevoLargo
    anchoDeHabitacion = nuevoAncho
  } 

  method estaAfinada() = self.habitacion() > 20

  method costo() = 2 * anchoDeHabitacion

  method esValiosa() = self.estaAfinada()
}

/*Violin*/
object violinStagg {
  
  var tremolo = 0

  var pintura = "laca acrílica"

  method tremolo(nuevosTremolos) {
    tremolo = nuevosTremolos
  }

  method pintura(nuevaPintura) {
    pintura = nuevaPintura
  }

  method estaAfinada() = tremolo < 10

  method costo() {
    return 15.max(20 - tremolo) 
  }

  method esCara() = self.costo() > 20

  method costoPar() = self.costo().even()

  method esValiosa() = pintura == "laca acrílica"
}