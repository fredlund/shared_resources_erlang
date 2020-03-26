package cc.carreteraRecurso;

public interface CarreteraRecurso {
  public Position enter(String cocheId, int velocidad);
  public Position move(String cocheId, int velocidad);
  public void exit(String cocheId, int velocidad);
  public void tick();
}
