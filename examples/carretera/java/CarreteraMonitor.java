package cc.carretera;
import es.upm.babel.cclib.Monitor;
import es.upm.aedlib.Entry;
import es.upm.aedlib.priorityqueue.PriorityQueue;
import es.upm.aedlib.priorityqueue.SortedListPriorityQueue;
import es.upm.aedlib.map.Map;
import es.upm.aedlib.map.HashTableMap;

public class CarreteraMonitor implements Carretera {
  final private int distance;
  final private int carriles;
  
  final private Monitor mutex;
  final private Monitor.Cond[] waitingToMove;
  final private PriorityQueue<Integer,String> moving;
  final private Map<String,Monitor.Cond> movingConds;
  
  // State
  private int time = 0;
  private String[][] cars;
  
  public CarreteraMonitor(int distance, int carriles) {
    this.distance = distance;
    this.carriles = carriles;
    mutex = new Monitor();
    waitingToMove = new Monitor.Cond[distance];
    for (int x=0; x<distance; x++)
      waitingToMove[x] = mutex.newCond();
    cars = new String[distance][carriles];
    moving = new SortedListPriorityQueue<Integer,String>();
    movingConds = new HashTableMap<String,Monitor.Cond>();
  }
  
  public Position enter(String car) {
    mutex.enter();
    Integer freeCarril = freeCarril(0);
    if (freeCarril == null) waitingToMove[0].await();
    freeCarril = freeCarril(0);
    cars[0][freeCarril] = car;
    mutex.leave();
    return new Position(0,freeCarril);
  }
  
  public Position move(String car) {
    mutex.enter();
    Position pos = position(car);
    Integer freeCarril = freeCarril(pos.getX()+1);
    if (freeCarril == null) waitingToMove[pos.getX()+1].await();
    freeCarril = freeCarril(pos.getX()+1);
    cars[pos.getX()+1][freeCarril] = car;
    freeCell(pos);
    mutex.leave();
    return new Position(pos.getX()+1,freeCarril);
  }
  
  public void exit(String car) {
    mutex.enter();
    Position pos = position(car);
    freeCell(pos);
    mutex.leave();
  }
  
  private void freeCell(Position pos) {
    cars[pos.getX()][pos.getY()] = null;
    Monitor.Cond waitCond = waitingToMove[pos.getX()];
    if (waitCond.waiting() > 0)
      waitCond.signal();
  }
  
  public void moving(String car, int velocidad) {
    mutex.enter();
    Position pos = position(car);
    moving.enqueue(velocidad+time,car);
    Monitor.Cond movingCond = getMovingCond(car);
    movingCond.await();
    signalMoving();
    mutex.leave();
  }
  
  public void tick() {
    mutex.enter();
    ++time;
    signalMoving();
    mutex.leave();
  }
  
  public void signalMoving() {
    if (moving.size() > 0) {
      Entry<Integer,String> first = moving.first();
      if (first.getKey() <= time) {
        Monitor.Cond movingCond = getMovingCond(first.getValue());
        if (movingCond.waiting() > 0) {
          movingCond.signal();
        }
        moving.dequeue();
      }
    }
  }
  
  private Monitor.Cond getMovingCond(String car) {
    Monitor.Cond movingCond = movingConds.get(car);
    if (movingCond == null) {
      movingCond = mutex.newCond();
      movingConds.put(car, movingCond);
    } 
    
    return movingCond;
  }
  
  private Position position(String car) {
    for (int x=0; x<distance; x++)
      for (int y=0; y<carriles; y++) {
        String otherCar = cars[x][y];
        if (otherCar != null && car.equals(otherCar))
          return new Position(x,y);
      }
    return null;
  }
  
  private Integer freeCarril(int x) {
    for (int y=0; y<carriles; y++)
      if (cars[x][y] == null) return y;
    return null;
  }
}
