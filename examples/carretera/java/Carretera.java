package cc.carretera;

public interface Carretera {
  public Position enter(String car);
  public Position move(String car);
  public void moving(String car, int velocidad);
  public void exit(String car);
  public void tick();
}
