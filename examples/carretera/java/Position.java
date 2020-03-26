package cc.carreteraRecurso;

public class Position {
  private int x;
  private int y;

  public Position(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public boolean equals(Object obj) {
    if (obj instanceof Position) {
      Position p = (Position) obj;
      return p.getX() == getX() && p.getY() == getY();
    } return false;
  }

  public int hashCode() {
    return getX()*100 + getY();
  }
}
