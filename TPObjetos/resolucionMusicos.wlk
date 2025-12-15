import instrumentos.*
/*Musicos*/
object johann {

  var instrumento = trompetaJupiter

  method esFeliz() = instrumento.esCara()

  method instrumento(nuevoInstrumento) {
    instrumento = nuevoInstrumento
  }
}

object wolfgang {

  var instrumento = guitarraFender

  method esFeliz() = johann.esFeliz()
}

object antonio {

  var instrumento = pianoBechstein

  method esFeliz() = instrumento.esValiosa()
}

object giuseppe {

  var instrumento = guitarraFender
  
  method esFeliz() = instrumento.estaAfinada() 

  method instrumento(nuevoInstrumento) {
    instrumento = nuevoInstrumento
  }
}

object maddalena {

  var instrumento = violinStagg

  method esFeliz() = instrumento.costoPar()
}