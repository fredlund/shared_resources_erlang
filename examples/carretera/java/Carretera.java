package cc.carretera;

public interface Carretera {

  // A car asks for permission to enter the carretera 
  // with the given velocity (less is higher).
  // Returns a new position to which the car may move.
  public Position enter(String car, int velocidad);

  // A car asks for passing to a new segment of the carretera
  // with the given velocity.
  // Returns a new position to which the car may move.
  public Position move(String car, int velocidad);

  // A car announces that it begins the move to the new permitted position.
  // Returns when the system (a sensor, not modelled) indicates that the
  // car has arrived.
  public void moving(String car);

  // A car announces that it leaves the carretera.
  public void exit(String car);

  // Time avances.
  public void tick();
}
