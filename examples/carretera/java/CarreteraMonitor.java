package cc.carretera;
import es.upm.babel.cclib.Monitor;
import es.upm.aedlib.Entry;
import es.upm.aedlib.priorityqueue.PriorityQueue;
import es.upm.aedlib.priorityqueue.SortedListPriorityQueue;
import es.upm.aedlib.map.Map;
import es.upm.aedlib.map.HashTableMap;
import es.upm.aedlib.Pair;
import es.upm.aedlib.set.Set;
import es.upm.aedlib.set.HashTableMapSet;
import es.upm.aedlib.indexedlist.IndexedList;
import es.upm.aedlib.indexedlist.ArrayIndexedList;
import java.util.Random;


public class CarreteraMonitor implements Carretera {

  // Resource State
  private final Map<String,Pair<Pos,Integer>> carretera;

  // Monitors and conditions
  final private Monitor mutex;
  final private Monitor.Cond[] waitingToMove;
  final private PriorityQueue<Integer,String> moving;
  final private Map<String,Monitor.Cond> movingConds;

  // Object state
  final private int segmentos;
  final private int carriles;
  final Random rnd;
  
  public CarreteraMonitor(int segmentos, int carriles) {
    this.segmentos = segmentos;
    this.carriles = carriles;
    this.rnd = new Random();

    mutex = new Monitor();
    waitingToMove = new Monitor.Cond[segmentos];
    for (int segmento=0; segmento<segmentos; segmento++)
      waitingToMove[segmento] = mutex.newCond();
    moving = new SortedListPriorityQueue<Integer,String>();
    movingConds = new HashTableMap<String,Monitor.Cond>();
    carretera = new HashTableMap<String,Pair<Pos,Integer>>();

    System.out.println("*** WARNING: you are running the solution");
  }

  public Pos entrar(String car, int tks) {
    mutex.enter();

    Integer freeCarril = freeCarril(1);

    if (freeCarril == null) waitingToMove[1-1].await();

    freeCarril = freeCarril(1);
    Pos pos = new Pos(1,freeCarril);
    carretera.put(car, new Pair<Pos,Integer>(pos,tks));

    mutex.leave();
    return pos;
  }

  public Pos avanzar(String car, int tks) {
    mutex.enter();

    Pos pos = pos(car);
    Integer freeCarril = freeCarril(pos.getSegmento()+1);

    if (freeCarril == null) waitingToMove[pos.getSegmento()+1-1].await();

    freeCarril = freeCarril(pos.getSegmento()+1);
    Pos newPos = new Pos(pos.getSegmento()+1,freeCarril);
    carretera.put(car, new Pair<Pos,Integer>(newPos,tks));

    signalPosFree(pos);

    mutex.leave();
    return newPos;
  }

  public void salir(String car) {
    mutex.enter();

    Pos pos = pos(car);
    carretera.remove(car);

    signalPosFree(pos);

    mutex.leave();
  }

  public void circulando(String car) {
    mutex.enter();

    Integer tksRemaining = tks(car);
    if (tksRemaining > 0) {
      Monitor.Cond movingCond = getMovingCond(car);
      movingCond.await();
    }

    signalMoving();

    mutex.leave();
  }

  public void tick() {
    mutex.enter();

    // Avoid problems with ConcurrentModificationException
    Set<String> domainCarretera = new HashTableMapSet<String>();
    for (String car : carretera.keys())
      domainCarretera.add(car);

    for (String car : domainCarretera) {
      Pair<Pos,Integer> pair = carretera.get(car);
      if (pair.getRight() > 0)
        carretera.put(car, new Pair<Pos,Integer>(pair.getLeft(),pair.getRight()-1));

    }

    signalMoving();

    mutex.leave();
  }

  // Signal that there is a free position for a segment
  private void signalPosFree(Pos pos) {
    Monitor.Cond waitCond = waitingToMove[pos.getSegmento()-1];
    if (waitCond.waiting() > 0)
      waitCond.signal();
  }

  // Signal moving cars that have moved sufficiently (tks == 0)
  public void signalMoving() {
    for (Entry<String,Pair<Pos,Integer>> entry : carretera.entries()) {
      if (entry.getValue().getRight() == 0) {
        Monitor.Cond movingCond = getMovingCond(entry.getKey());
        if (movingCond.waiting() > 0) {
          movingCond.signal();
          // Para profesores de programacion II : cierro los ojos!
          break; 
        }
      }
    }
  }

  // Find the condition for a car which is signalled when it has finished moving
  private Monitor.Cond getMovingCond(String car) {
    Monitor.Cond movingCond = movingConds.get(car);
    if (movingCond == null) {
      movingCond = mutex.newCond();
      movingConds.put(car, movingCond);
    }

    return movingCond;
  }

  // Returns the position of a car in the carretera
  private Pos pos(String car) {
    Pair<Pos,Integer> pair = carretera.get(car);
    return pair.getLeft();
  }

  // Returns the ticks remaining for a car in the carretera
  private Integer tks(String car) {
    Pair<Pos,Integer> pair = carretera.get(car);
    return pair.getRight();
  }

  // Return a free carril at segmento, or null if there is no free carril
  private Integer freeCarril(int segmento) {
    Set<Integer> occupiedCarriles = new HashTableMapSet<Integer>();
    IndexedList<Integer> freeCarriles = new ArrayIndexedList<Integer>();

    for (String car : carretera.keys()) {
      Pos pos = carretera.get(car).getLeft();
      if (pos.getSegmento() == segmento)
        occupiedCarriles.add(pos.getCarril());
    }

    for (int i=1; i<=carriles; i++) {
      if (!occupiedCarriles.contains(i))
        freeCarriles.add(freeCarriles.size(),i);
    }

    // Nondeterministic reply for better testing
    if (freeCarriles.size() > 0) {
      int carrilIndex = rnd.nextInt(freeCarriles.size());
      return freeCarriles.get(carrilIndex);
    } else return null;
  }
}
