package cc.carretera;

public class Car {
  private String id;
  private int velocidad;
  private int xLimit;
  private CarreteraRecurso cr;

  public Car(String id, int velocidad, int xLimit, CarreteraRecurso crLimit) {
    this.id = id;
    this.velocidad = velocidad;
    this.xLimit = xLimit;
    this.cr = cr;
  }

  public String getId() {
    return id;
  }

  public int getVelocidad() {
    return velocidad;
  }

  public void run() {
    Position p = enter(getId(), getVelocidad());
    while (p.getX() < xLimit()) {
      move(getId(), getVelocidad());
    }
    exit(getId(),getVelocidad());
  }
}

