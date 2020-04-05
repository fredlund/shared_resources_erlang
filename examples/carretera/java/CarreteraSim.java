/*
 * Simulates a carretera in a GUI window.
 *
 */
package cc.carretera;

import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.JTextArea;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Random;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.swing.border.LineBorder;
import java.util.Map;
import java.util.HashMap;
import java.awt.Color;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.JTextField;
import javax.swing.Box;
import javax.swing.JLabel;
import java.awt.FlowLayout;
import javax.swing.JScrollPane;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.SwingWorker;
import java.awt.Component;
import javax.swing.JCheckBox;
import java.util.function.Supplier;


public class CarreteraSim {
  
  // Dimensions
  int distance;
  int carriles;

  // Random number generation
  Random rnd;

  // GUI state
  private JFrame frmCarreterasim;
  JTextArea callsTextArea;
  JLabel[][] carretera;
  JLabel timeLab;
  
  // Current time
  int time = 0;
  
  // Simulation
  Sim sim;
  
  // For sending messages to simulation from GUI
  BlockingQueue<Integer> tickQueue;
  
  // Manually step ticks or not
  boolean stepTicks = false;
  
  // Current generation -- we keep a count of the number of times
  // the simulation was started to keep from displaying spurious messages
  int generation = 0;
  
  
  /**
   * Launch the application.
   */
  public static void main(String[] args) {
    EventQueue.invokeLater(new Runnable() {
        public void run() {
          try {
            CarreteraSim window = new CarreteraSim();
            window.frmCarreterasim.setVisible(true);
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      });
  }
  
  /**
   * Create the application.
   */
  public CarreteraSim() {
    initialize();
  }
  
  /**
   * Setup the GUI.
   */
  private void initialize() {
    rnd = new Random();
    distance = 2+rnd.nextInt(4);
    carriles = 1+rnd.nextInt(3);
    
    frmCarreterasim = new JFrame();
    frmCarreterasim.setTitle("CarreteraSim");
    frmCarreterasim.setBounds(100, 100, Math.max(400,200+100*distance), Math.max(500,300+carriles*100));
    frmCarreterasim.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    JLabel lblTime = new JLabel("Time:"); 
    JLabel timeLab = new JLabel("0"); this.timeLab = timeLab;
    JCheckBox stepTicksBox = new JCheckBox("Step ticks");
    
    JButton btnDoTimeTick = new JButton("Tick");
    btnDoTimeTick.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          try {
            tickQueue.put(1);
          } catch (InterruptedException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
        }
      });
    btnDoTimeTick.setEnabled(false);
    carretera = new JLabel[distance][carriles];
    for (int x=0; x<distance; x++)
      for (int y=0; y<carriles; y++)
        carretera[x][y] = new JLabel("--------");

    JPanel panel_options = new JPanel();
    JPanel panel_carretera = new JPanel();
    panel_carretera.setBorder(new LineBorder(new Color(0, 0, 0)));

    JPanel panel_actions = new JPanel();
    JPanel panel_calls = new JPanel();
    
    JTextField txtCalls = new JTextField();
    txtCalls.setEditable(false);
    txtCalls.setText("Calls:");
    txtCalls.setColumns(10);
    
    callsTextArea = new JTextArea();
    callsTextArea.setColumns(20);
    callsTextArea.setRows(20);
    JScrollPane callsTextAreaSP = new JScrollPane(callsTextArea);
    
    JButton btnQuit = new JButton("Quit");
    btnQuit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          frmCarreterasim.dispose();
          System.exit(0);
        }
      });
    
    JButton btnPauseSim = new JButton("Pause simulation");
    btnPauseSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          try {
            tickQueue.put(-1);
            if (btnPauseSim.getText().equals("Pause simulation"))
              btnPauseSim.setText("Restart simulation");
            else
              btnPauseSim.setText("Pause simulation");
          } catch (InterruptedException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
        }
      });
    btnPauseSim.setEnabled(false);
    
    JButton btnStartSim = new JButton("Start simulation");
    final CarreteraSim win = this;
    btnStartSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          
          ++generation;
          
          if (sim != null && tickQueue != null) {
            try {
              tickQueue.put(-10);
            } catch (InterruptedException e1) {
              // TODO Auto-generated catch block
              e1.printStackTrace();
            }
          }
          
          tickQueue = new LinkedBlockingQueue<Integer>();
          time = 0;
          timeLab.setText(Integer.valueOf(time).toString());
          sim = new Sim(win,rnd,generation,tickQueue,distance,carriles);
          stepTicks = stepTicksBox.isSelected();
          btnDoTimeTick.setEnabled(stepTicks);
          btnPauseSim.setEnabled(!stepTicks);
          btnPauseSim.setText("Pause simulation");
          
          for (JLabel[] lblRow : carretera)
            for (JLabel lbl : lblRow)
              lbl.setText("--------");
          callsTextArea.setText("");
          
          sim.execute();
        }
      });
    
    // Top panel layout
    GroupLayout gl_top = new GroupLayout(frmCarreterasim.getContentPane());
    gl_top.setAutoCreateGaps(true);
    gl_top.setAutoCreateContainerGaps(true);
    gl_top.setHorizontalGroup
      (
       gl_top.createParallelGroup(Alignment.CENTER)
       .addComponent(panel_calls)
       .addComponent(panel_actions)
       .addComponent(panel_options)
       .addComponent(panel_carretera)
       );
    
    gl_top.setVerticalGroup
      (
       gl_top.createSequentialGroup()
       .addComponent(panel_options)
       .addComponent(panel_carretera)
       .addComponent(panel_calls)
       .addComponent(panel_actions)
       );
    
    
    // Panel 1: carretera
    
    GroupLayout gl_panel_carretera = new GroupLayout(panel_carretera);
    GroupLayout.ParallelGroup[] horizontalGroups = new GroupLayout.ParallelGroup[distance];
    GroupLayout.ParallelGroup[] verticalGroups = new GroupLayout.ParallelGroup[carriles];

    for (int x=0; x<distance; x++) {
      GroupLayout.ParallelGroup g =
        gl_panel_carretera.createParallelGroup(Alignment.LEADING);
      horizontalGroups[x] =
        g;
      for (int y=0; y<carriles; y++)
        g.addComponent(carretera[x][y], GroupLayout.DEFAULT_SIZE, 100, Short.MAX_VALUE);
    }

    for (int y=0; y<carriles; y++) {
      GroupLayout.ParallelGroup g =
        gl_panel_carretera.createParallelGroup(Alignment.BASELINE);
      verticalGroups[y] =
        g;
      for (int x=0; x<distance; x++)
        g.addComponent(carretera[x][y], GroupLayout.DEFAULT_SIZE, 30, Short.MAX_VALUE);
    }

    gl_panel_carretera.setAutoCreateGaps(true);
    gl_panel_carretera.setAutoCreateContainerGaps(true);
    GroupLayout.SequentialGroup horizontalGroup =
      gl_panel_carretera.createSequentialGroup();
    for (int x=0; x<distance; x++)
      horizontalGroup.addGroup(horizontalGroups[x]);
    gl_panel_carretera.setHorizontalGroup(horizontalGroup);

    GroupLayout.SequentialGroup verticalGroup =
      gl_panel_carretera.createSequentialGroup();
    for (int y=0; y<carriles; y++)
      verticalGroup.addGroup(verticalGroups[y]);
    gl_panel_carretera.setVerticalGroup(verticalGroup);
    panel_carretera.setLayout(gl_panel_carretera);
    
    
    // Panel time_options: time and time tick option
    
    GroupLayout gl_panel_options = new GroupLayout(panel_options);
    gl_panel_options.setAutoCreateGaps(true);
    gl_panel_options.setAutoCreateContainerGaps(true);
    gl_panel_options.setHorizontalGroup
      (
       gl_panel_options.createSequentialGroup()
       .addComponent(lblTime)
       .addComponent(timeLab)
       //.addPreferredGap(ComponentPlacement.UNRELATED)
       .addGap(150)
       .addComponent(stepTicksBox)
       );
    gl_panel_options.setVerticalGroup
      (
       gl_panel_options.createParallelGroup(Alignment.BASELINE)
       .addComponent(lblTime)
       .addComponent(timeLab)
       .addComponent(stepTicksBox)
       );
    panel_options.setLayout(gl_panel_options);
    
    
    
    // Panel actions: tick button, start simulation and quit
    
    GroupLayout gl_panel_actions = new GroupLayout(panel_actions);
    gl_panel_actions.setAutoCreateGaps(true);
    gl_panel_actions.setAutoCreateContainerGaps(true);
    gl_panel_actions.setHorizontalGroup
      (
       gl_panel_actions.createSequentialGroup()
       .addComponent(btnDoTimeTick)
       .addPreferredGap(ComponentPlacement.UNRELATED)
       .addComponent(btnPauseSim)
       .addComponent(btnStartSim)
       .addPreferredGap(ComponentPlacement.UNRELATED)
       .addComponent(btnQuit)
       );
    gl_panel_actions.setVerticalGroup
      (
       gl_panel_actions.createParallelGroup(Alignment.BASELINE)
       .addComponent(btnQuit)
       .addComponent(btnPauseSim)
       .addComponent(btnStartSim)
       .addComponent(btnDoTimeTick)
       );
    panel_actions.setLayout(gl_panel_actions);
    
    
    // Panel calls: calls text window and label
    
    GroupLayout gl_panel_calls = new GroupLayout(panel_calls);
    gl_panel_calls.setAutoCreateGaps(true);
    gl_panel_calls.setAutoCreateContainerGaps(true);
    
    gl_panel_calls.setHorizontalGroup
      (
       gl_panel_calls.createParallelGroup(Alignment.LEADING)
       .addComponent(txtCalls)
       .addComponent(callsTextAreaSP)
       );
    gl_panel_calls.setVerticalGroup
      (
       gl_panel_calls.createSequentialGroup()
       .addComponent(txtCalls, GroupLayout.PREFERRED_SIZE, 20, GroupLayout.PREFERRED_SIZE)
       .addComponent(callsTextAreaSP, GroupLayout.PREFERRED_SIZE, 200, GroupLayout.PREFERRED_SIZE)
       );
    panel_calls.setLayout(gl_panel_calls);
    
    
    // Set layout on top content pane
    
    frmCarreterasim.getContentPane().setLayout(gl_top);
  }
}


/*
 * Run the simulation. Since we can change the GUI only in a single
 * thread the simulation is run as a "SwingWorker", 
 * and we send simulation events
 * back to the GUI theread.
 */


class Sim extends SwingWorker<Void,Object> {
  
  // Simulation cars
  String[] cars = {"vw", "seat", "volvo", "toyota", "fiat", "ford", "citroen", "porsche"};
  
  // Car velocicities (lower is faster!)
  Map<String,Integer> velocidades;
  
  // Main application state (including GUI)
  CarreteraSim cs;
  
  // Current generation
  int generation;
  
  // Messages from GUI
  BlockingQueue<Integer> tickQueue;
  
  // Random state
  Random rnd;
  
  // Dimensions of carretera
  int distance;
  int carriles;
  
  
  Sim(CarreteraSim cs, Random rnd, int generation, BlockingQueue<Integer> tickQueue, int distance, int carriles) {
    this.cs = cs;
    this.generation = generation;
    this.tickQueue = tickQueue;
    this.distance = distance;
    this.carriles = carriles;
    this.rnd = rnd;
    
    // Set car velocities (vw is punished for "dieselgate"...)
    this.velocidades = new HashMap<>();
    velocidades.put("vw",4); velocidades.put("seat",3); velocidades.put("volvo",1);
    velocidades.put("toyota",1); velocidades.put("fiat",2); velocidades.put("ford",1);
    velocidades.put("citroen",2); velocidades.put("porsche",3); 
  }
  
  // Remove a car from the GUI
  static void removeCar(CarreteraSim cs, String car) {
    for (int i=0; i<cs.carretera.length; i++)
      for (int j=0; j<cs.carretera[0].length; j++)
        if (cs.carretera[i][j].getText().startsWith(car))
          cs.carretera[i][j].setText("--------");
  }
  
  // Handles the GUI updates resulting from simulation events
  @Override
  protected void process(List<Object> messages) {
    for (Object preMsg : messages) {
      if (preMsg instanceof String) {
        String str = (String) preMsg;
        cs.callsTextArea.append(str+"\n");
        System.out.println(str);
      } else if (preMsg instanceof CallAndGeneration) {
        CallAndGeneration msg = (CallAndGeneration) preMsg;
        if (msg.generation == generation) {
          Call call = msg.call;
          
          if (call.raisedException) {
            String str = "\n*** Error: exception thrown:\n"+call.exception;
            System.out.println(str);
            call.exception.printStackTrace();
            cs.callsTextArea.append(str+"\n");
            for (StackTraceElement e : call.exception.getStackTrace()) {
              cs.callsTextArea.append(e+"\n");
            }
          } else if (call.failed) {
            String str = "\n*** Error: "+call.failMessage;
            cs.callsTextArea.append(str+"\n");
            System.out.println(str);
          } else {
            String str = cs.time+": "+call;
            cs.callsTextArea.append(str+"\n");
            System.out.println(str);
            
            if (call.name.equals("enter") && call.returned) {
              Position pos = call.result;
              JLabel lbl = cs.carretera[pos.getX()][pos.getY()];
              lbl.setText(call.car);
            } else if (call.name.equals("move") && call.returned) {
              Position pos = call.result;
              removeCar(cs,call.car);
              JLabel lbl = cs.carretera[pos.getX()][pos.getY()];
              lbl.setText(call.car);
            } else if (call.name.equals("exit") && call.returned) {
              removeCar(cs,call.car);
            } else if (call.name.equals("moving") && !call.returned) {
              Position pos = call.result;
              for (int i=0; i<cs.carretera.length; i++)
                for (int j=0; j<cs.carretera[0].length; j++)
                  if (cs.carretera[i][j].getText().equals(call.car))
                    cs.carretera[i][j].setText(call.car+"@"+Integer.valueOf(cs.time+call.velocidad));
            } else if (call.name.equals("tick") && call.returned) {
              ++cs.time;
              cs.timeLab.setText(Integer.valueOf(cs.time).toString());
            }
          }
        }
        
      } else {
        String str = "\n*** Internal error: unknown message "+preMsg+" received";
        cs.callsTextArea.append(str+"\n");
        System.out.println(str);
      }
    }
  }
  
  // Main simulation thread
  @Override
  protected Void doInBackground() throws Exception {
    boolean stepTicks = cs.stepTicks;
    AtomicBoolean terminated = new AtomicBoolean(false);
    
    // Shuffle cars
    for (int i=0; i<cars.length*5; i++) {
      int one = rnd.nextInt(cars.length);
      int two = rnd.nextInt(cars.length);
      String carOne = cars[one];
      cars[one] = cars[two];
      cars[two] = carOne;
    }
    
    // Invoke the monitor
    Carretera crPre = null;
    
    try {
      crPre = new CarreteraMonitor(distance,carriles);
    } catch (Throwable exc) {
      String str =
        "\n*** Error: calling CarreteraMonitor("+distance+","+carriles+") raised the exception "+exc;
      for (StackTraceElement e : exc.getStackTrace())
        str += e.toString()+"\n";
      publish(str);
      return null;
    }

    // This strange looking code is to pass a Java check that variables used in
    // lambda expressions must be final or effectively final. Since crPre is set
    // in the try it does not pass the test (even if we assign crPre also in the catch part)
    Carretera cr = crPre;

    // Number of cars to simulate
    int numCars = rnd.nextInt(cars.length-1)+1;
    AtomicInteger carsToExit = new AtomicInteger(numCars);
    
    if (distance < 1 || carriles < 1) {
      System.out.println
        ("\n*** Error: distance and carriles cannot be smaller than 1");
      System.exit(1);
    }
    
    System.out.println
      ("Simulation of "+numCars+" cars moving in a carretera of distance "
       +distance+" with "+carriles+" lanes");
    
    for (int i=0; i<numCars; i++) {
      String car = cars[i];
      int velocidad = velocidades.get(car);
      
      // One thread per car executes the car protocol (enter, moving, [move, moving]*, exit)
      Thread carTh = new Thread(car) {
          public void run() {
            Position result = null;
            int currX = 0;
            
            // Do the car process
            if (!terminated.get()) {
              terminated.compareAndSet
                (false,!doResultCall(() -> cr.enter(car), Call.enter(car), currX, carriles));
            }
            
            if (!terminated.get()) {
              terminated.compareAndSet
                (false,!doCall(() -> cr.moving(car,velocidad), Call.moving(car,velocidad)));
            }
            
            while (!terminated.get() && currX < distance - 1) {
              
              if (!terminated.get()) {
                terminated.compareAndSet
                  (false,!doResultCall(() -> cr.move(car), Call.move(car), ++currX, carriles));
              }
              
              if (!terminated.get()) {
                terminated.compareAndSet
                  (false,!doCall(() -> cr.moving(car,velocidad), Call.moving(car,velocidad)));
              }
            }
            
            if (!terminated.get()) {
              terminated.compareAndSet
                (false,!doCall(() -> cr.exit(car), Call.exit(car)));
            }
            
            carsToExit.decrementAndGet();
          }
        };
      carTh.start();
    }
    
    
    // Avance time -- either manualy (step ticks) or automatically.
    // Listens to orders from the GUI to pause or quit the current simulation.
    Thread timeThread = new Thread("tick") {
        public void run() {
          do {
            Integer cmd = null;
            if (stepTicks) {
              try {
                cmd = tickQueue.take();
                terminated.compareAndSet(false,cmd != null && cmd == -10);
              } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
            } else {
              try {
                Thread.sleep(5000);
                
                cmd = tickQueue.poll();
                boolean stopped = (cmd != null && cmd == -1);
                terminated.compareAndSet(false,cmd != null && cmd == -10);
                
                while (stopped && !terminated.get()) {
                  cmd = tickQueue.take();
                  terminated.compareAndSet(false,cmd != null && cmd == -10);
                  stopped = !(cmd != null && cmd == -1);
                }
              } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
            }
            
            if (!terminated.get()) {
              terminated.compareAndSet(false,!doCall(() -> cr.tick(), Call.tick()));
            }
          } while (!terminated.get() && carsToExit.get() > 0);
        }
      };	
    timeThread.start();
    return null;
  }
  
  // Send a message from the simulation to the GUI
  Call sendToGUI(Call call) {
    publish(new CallAndGeneration(call,generation));
    return call;
  }
  
  boolean doCall(Runnable callCode, Call oldCall) {
    sendToGUI(oldCall);
    Call call = new Call(oldCall);
    
    boolean callResult = true;
    
    try {
      callCode.run();
    } catch (Throwable exc) {
      call.raisedException = true;
      call.exception = exc;
      callResult = false;
    };
    
    if (callResult) {
      call.returned();
    }
    
    sendToGUI(call);
    return callResult;
  }
  
  boolean doResultCall(Supplier<Position> callCode, Call oldCall, int expectedX, int carriles) {
    sendToGUI(oldCall);
    Call call = new Call(oldCall);
    boolean callResult = true;
    Position position = null;
    
    try {
      position = callCode.get();
    } catch (Throwable exc) {
      call.raisedException = true;
      call.exception = exc;
      callResult = false;
    };
    
    if (callResult) {
      call.returned(position);
      callResult = checkCall(call, expectedX, carriles);
    }
    
    sendToGUI(call);
    return callResult;
  }
  
  private boolean checkCall(Call call, int expectedX, int carriles) {
    Position result = call.result;
    
    if (result == null) {
      call.failed = true;
      call.failMessage =
        "The call to "+call.getCallString()+" returned a NULL value";
      return false;
    } else if (result.getX() != expectedX) {
      call.failed = true;
      call.failMessage =
        "The call to "+call.getCallString()+" returned a X coordinate "+
        result.getX()+" != expected value "+expectedX;
      return false;
    } else  if (result.getY() < 0 || result.getY() >= carriles) {
      call.failed = true;
      call.failMessage =
        "he call to "+call.getCallString()+" returned a Y coordinate "+
        result.getY()+" < 0 or >= the number of carriles = "+carriles;
      return false;
    } else return true;
  }
}

// A simulation event sent to the GUI which includes the generation --
// to discard "old" events.
class CallAndGeneration {
  Call call;
  Integer generation;
  
  CallAndGeneration(Call call, int generation) {
    this.call = call;
    this.generation = generation;
  }
}


// A simulation event sent to the GUI
class Call {
  String name;
  String car=null;
  Integer velocidad=null;
  boolean returned;
  Position result=null;
  boolean failed=false;
  String failMessage=null;
  boolean raisedException=false;
  Throwable exception;
  
  Call(String name) { this.name = name; this.returned = false; }
  
  Call(Call call) {
    this.name = call.name;
    this.car = call.car;
    this.velocidad = call.velocidad;
    this.returned = call.returned;
    this.result = call.result;
    this.failed = call.failed;
    this.failMessage = call.failMessage;
    this.raisedException = call.raisedException;
    this.exception = call.exception;
  }
  
  static Call enter(String car) {
    Call call = new Call("enter"); call.car = car; return call;
  }
  
  static Call move(String car) {
    Call call = new Call("move"); call.car = car; return call;
  }
  
  static Call exit(String car) {
    Call call = new Call("exit"); call.car = car; return call;
  }
  
  static Call moving(String car, int velocidad) {
    Call call = new Call("moving"); call.car = car; call.velocidad = velocidad; return call;
  }
  
  static Call tick() {
    Call call = new Call("tick"); return call;
  }
  
  public void returned() {
    this.returned = true;
  }
  
  public void returned(Position result) {
    this.returned = true;
    this.result = result;
  }
  
  public String getCallString() {
    String str = name+"(";
    if (car != null) str +=car;
    if (velocidad != null) str +=","+velocidad;
    str += ")";
    return str;
  }
  
  public String toString() {
    String str = getCallString();
    if (returned) {
      str += " returned";
      if (result != null) str += " "+result;
    }
    return str;
  }
}



