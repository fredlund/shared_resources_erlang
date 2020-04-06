package cc.carretera;

public interface Carretera {
  public Position enter(String car, int velocidad);
  public Position move(String car, int velocidad);
  public void moving(String car);
  public void exit(String car);
  public void tick();
}
