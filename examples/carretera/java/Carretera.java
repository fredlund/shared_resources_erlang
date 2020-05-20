package cc.carretera;

public interface Carretera {
  /**
   * Un coche pide permiso para entrar en el primer segmento de la
   * carretera con una determinada velocidad
   *
   * @param id identificador del coche
   * @param tks número de ticks necearios para atravesar un segmento (velocidad)
   *
   * @return posición (segmento/carril) que ocupa el coche, ver clase Pos
   */
  Pos entrar(String id, int tks);

  /**
   * Un coche pide permiso para entrar en el siguiente segmento con
   * una determinada velocidad.
   *
   * @param id identificador del coche
   * @param tks número de ticks necearios para atravesar un segmento (velocidad)
   *
   * @return posición (segmento/carril) que ocupa el coche, ver clase Pos
   */
  Pos avanzar(String id, int tks);

  /**
   * Un coche "circula" a lo largo del segmento en el que está. La
   * operación termina cuando el coche ha llegado al final del segmento.
   *
   * @param id identificador del coche
   */
  void circulando(String id);

  /**
   * Un coche abandona el último segmento.
   *
   * @param id identificador del coche
   */
  void salir(String id);

  /**
   * Hace avanzar el tiempo de forma que a cada coche en la carretera
   * le queda un tick menos para llegar al final de su segmento.
   */
  void tick();
}
