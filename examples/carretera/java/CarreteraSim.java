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

import javax.swing.border.LineBorder;

import es.upm.aedlib.map.HashTableMap;
import es.upm.aedlib.map.Map;

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

public class CarreteraSim {
  
  private JFrame frmCarreterasim;
  JTextArea callsTextArea;
  
  JLabel[][] carretera;
  int time = 0;
  JLabel timeLab;
  Sim sim;
  BlockingQueue<Integer> tickQueue;
  boolean stepTicks = false;
  
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
   * Initialize the contents of the frame.
   */
  private void initialize() {
    frmCarreterasim = new JFrame();
    frmCarreterasim.setTitle("CarreteraSim");
    frmCarreterasim.setBounds(100, 100, 450, 300);
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
    
    JLabel lbl0_1 = new JLabel("--------");
    JLabel lbl1_1 = new JLabel("--------");
    JLabel lbl2_1 = new JLabel("--------");
    JLabel lbl3_1 = new JLabel("--------");
    JLabel lbl0_0 = new JLabel("--------");
    JLabel lbl1_0 = new JLabel("--------");
    JLabel lbl2_0 = new JLabel("--------");
    JLabel lbl3_0 = new JLabel("--------");
    
    carretera = new JLabel[][] { { lbl0_0, lbl0_1}, { lbl1_0, lbl1_1 }, { lbl2_0, lbl2_1 }, { lbl3_0, lbl3_1 } };
    
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
    callsTextArea.setColumns(40);
    callsTextArea.setRows(20);
    JScrollPane callsTextAreaSP = new JScrollPane(callsTextArea);
    
    JButton btnQuit = new JButton("Quit");
    btnQuit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          frmCarreterasim.dispose();
          System.exit(0);
        }
      });
    JButton btnStartSim = new JButton("Start simulation");
    final CarreteraSim win = this;
    btnStartSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          tickQueue = new LinkedBlockingQueue<Integer>();
          time = 0;
          timeLab.setText(Integer.valueOf(time).toString());
          sim = new Sim(win);
          tickQueue = tickQueue;
          stepTicks = stepTicksBox.isSelected();
          btnDoTimeTick.setEnabled(stepTicks);
          sim.execute();
        }
      });
    
    // Top panel layout
    GroupLayout gl_top = new GroupLayout(frmCarreterasim.getContentPane());
    gl_top.setHorizontalGroup
      (
       gl_top.createParallelGroup(Alignment.TRAILING)
       .addGroup(gl_top.createSequentialGroup()
                 .addGroup(gl_top.createParallelGroup(Alignment.LEADING)
                           .addComponent(panel_calls, GroupLayout.PREFERRED_SIZE, 450, GroupLayout.PREFERRED_SIZE)
                           .addComponent(panel_actions, GroupLayout.PREFERRED_SIZE, 450, GroupLayout.PREFERRED_SIZE)
                           .addGroup(gl_top.createSequentialGroup()
                                     .addContainerGap()
                                     .addComponent(panel_options, GroupLayout.PREFERRED_SIZE, 450, Short.MAX_VALUE))
                           .addGroup(gl_top.createSequentialGroup()
                                     .addContainerGap()
                                     .addComponent(panel_carretera, GroupLayout.PREFERRED_SIZE, 450, GroupLayout.PREFERRED_SIZE)))
                 .addContainerGap())
       );
    
    gl_top.setVerticalGroup
      (
       gl_top.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_top.createSequentialGroup()
                 .addComponent(panel_options, GroupLayout.PREFERRED_SIZE, 41, GroupLayout.PREFERRED_SIZE)
                 .addPreferredGap(ComponentPlacement.UNRELATED)
                 .addComponent(panel_carretera, GroupLayout.PREFERRED_SIZE, 106, GroupLayout.PREFERRED_SIZE)
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addComponent(panel_calls, GroupLayout.PREFERRED_SIZE, 68, GroupLayout.PREFERRED_SIZE)
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addComponent(panel_actions, GroupLayout.DEFAULT_SIZE, 36, Short.MAX_VALUE)
                 .addGap(6))
       );
    

    // Panel 1: carretera

    GroupLayout gl_panel_carretera = new GroupLayout(panel_carretera);
    gl_panel_carretera.setHorizontalGroup
      (
       gl_panel_carretera.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_panel_carretera.createSequentialGroup()
                 .addGap(20)
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.LEADING)
                           .addComponent(lbl0_0, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE)
                           .addComponent(lbl0_1, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE))
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.LEADING)
                           .addComponent(lbl1_1, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE)
                           .addComponent(lbl1_0, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE))
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.LEADING)
                           .addComponent(lbl2_1, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE)
                           .addComponent(lbl2_0, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE))
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.LEADING)
                           .addComponent(lbl3_0, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE)
                           .addComponent(lbl3_1, GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE))
                 .addGap(10))
       );
    gl_panel_carretera.setVerticalGroup
      (
       gl_panel_carretera.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_panel_carretera.createSequentialGroup()
                 .addGap(20)
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.BASELINE)
                           .addComponent(lbl0_1, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl1_1, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl3_1, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl2_1, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE))
                 .addGroup(gl_panel_carretera.createParallelGroup(Alignment.BASELINE, false)
                           .addComponent(lbl0_0, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl1_0, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl2_0, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE)
                           .addComponent(lbl3_0, GroupLayout.PREFERRED_SIZE, 31, GroupLayout.PREFERRED_SIZE))
                 .addGap(10))
       );
    panel_carretera.setLayout(gl_panel_carretera);
    

    // Panel time_options: time and time tick option

    GroupLayout gl_panel_options = new GroupLayout(panel_options);
    gl_panel_options.setHorizontalGroup
      (
       gl_panel_options.createSequentialGroup()
       .addComponent(lblTime)
       .addComponent(timeLab)
       .addComponent(stepTicksBox)
       );
    gl_panel_options.setVerticalGroup
      (
       gl_panel_options.createParallelGroup(Alignment.BASELINE)
       .addGroup(gl_panel_options.createSequentialGroup()
                 .addComponent(lblTime)
                 .addComponent(timeLab))
                 .addComponent(stepTicksBox)
      );
    panel_options.setLayout(gl_panel_options);
    
    

    // Panel actions: tick button, start simulation and quit
    
    GroupLayout gl_panel_actions = new GroupLayout(panel_actions);
    gl_panel_actions.setHorizontalGroup
      (
       gl_panel_actions.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_panel_actions.createSequentialGroup()
                 .addGap(10)
                 .addComponent(btnDoTimeTick)
                 .addPreferredGap(ComponentPlacement.RELATED, 238, Short.MAX_VALUE)
                 .addComponent(btnStartSim)
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addComponent(btnQuit)
                 .addContainerGap())
       );
    gl_panel_actions.setVerticalGroup
      (
       gl_panel_actions.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_panel_actions.createSequentialGroup()
                 .addGap(5)
                 .addGroup(gl_panel_actions.createParallelGroup(Alignment.BASELINE)
                           .addComponent(btnQuit)
                           .addComponent(btnStartSim)
                           .addComponent(btnDoTimeTick)))
       );
    panel_actions.setLayout(gl_panel_actions);


    // Panel calls: calls text window and label

    GroupLayout gl_panel_calls = new GroupLayout(panel_calls);
    gl_panel_calls.setHorizontalGroup
      (
       gl_panel_calls.createParallelGroup(Alignment.LEADING)
       .addGroup(Alignment.TRAILING, gl_panel_calls.createSequentialGroup()
                 .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                 .addComponent(txtCalls, GroupLayout.PREFERRED_SIZE, 100, GroupLayout.PREFERRED_SIZE)
                 .addContainerGap())
       .addGroup(gl_panel_calls.createSequentialGroup()
                 .addComponent(callsTextAreaSP, GroupLayout.PREFERRED_SIZE, 100, GroupLayout.PREFERRED_SIZE)
                 .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
       );
    gl_panel_calls.setVerticalGroup
      (
       gl_panel_calls.createParallelGroup(Alignment.LEADING)
       .addGroup(gl_panel_calls.createSequentialGroup()
                 .addComponent(txtCalls, GroupLayout.PREFERRED_SIZE, 34, GroupLayout.PREFERRED_SIZE)
                 .addPreferredGap(ComponentPlacement.RELATED)
                 .addComponent(callsTextAreaSP, GroupLayout.PREFERRED_SIZE, 68, GroupLayout.PREFERRED_SIZE)
                 .addGap(47))
       );
    panel_calls.setLayout(gl_panel_calls);
    

    // Set layout on top content pane
    
    frmCarreterasim.getContentPane().setLayout(gl_top);
  }
}

class Sim extends SwingWorker<Void,Object> {
  Random rnd;
  String[] cars = {"vw", "seat", "volvo", "toyota", "fiat", "ford", "citroen", "porsche"};
  Map<String,Integer> velocidades;
  int numCars;
  CarreteraSim cs;
  
  Sim(CarreteraSim cs) {
    this.cs = cs;
    this.velocidades = new HashTableMap<>();
    velocidades.put("vw",4); velocidades.put("seat",3); velocidades.put("volvo",1);
    velocidades.put("toyota",1); velocidades.put("fiat",2); velocidades.put("ford",1);
    velocidades.put("citroen",2); velocidades.put("porsche",3); 
  }
  
  static void removeCar(CarreteraSim cs, String car) {
    for (int i=0; i<cs.carretera.length; i++)
      for (int j=0; j<cs.carretera[0].length; j++)
        if (cs.carretera[i][j].getText().startsWith(car))
          cs.carretera[i][j].setText("--------");
  }
  
  @Override
  protected void process(List<Object> messages) {
    for (Object msg : messages) {
      System.out.println("received "+msg);
      if (msg instanceof Call) {
        Call call = (Call) msg;
        cs.callsTextArea.append(cs.time+": "+call+"\n");
        if (call.name.equals("enter") && call.returned && call.result instanceof Position) {
          Position pos = (Position) call.result;
          JLabel lbl = cs.carretera[pos.getX()][pos.getY()];
          if (call.parm1 instanceof String) {
            String carName = (String) call.parm1;
            System.out.println("setting "+carName+" at enter");
            lbl.setText(carName);
          }
        } if (call.name.equals("move") && call.returned && call.result instanceof Position) {
          Position pos = (Position) call.result;
          if (call.parm1 instanceof String) {
            String carName = (String) call.parm1;
            removeCar(cs,carName);
            JLabel lbl = cs.carretera[pos.getX()][pos.getY()];
            System.out.println("setting "+carName+" at move");
            lbl.setText(carName);
          }
        } if (call.name.equals("exit") && call.returned) {
          if (call.parm1 instanceof String) {
            String carName = (String) call.parm1;
            removeCar(cs,carName);
          }
        } if (call.name.equals("moving")) {
          Position pos = (Position) call.result;
          if (call.parm1 instanceof String && call.parm2 instanceof Integer) {
            String carName = (String) call.parm1;
            Integer velocidad = (Integer) call.parm2;
            for (int i=0; i<cs.carretera.length; i++)
              for (int j=0; j<cs.carretera[0].length; j++)
                if (cs.carretera[i][j].getText().equals(carName))
                  cs.carretera[i][j].setText(carName+"@"+Integer.valueOf(cs.time+velocidad));
          }
        } else if (call.name.equals("tick") && call.returned) {
          ++cs.time;
          cs.timeLab.setText(Integer.valueOf(cs.time).toString());
        }
      }
    }
  }
  
  @Override
  protected Void doInBackground() throws Exception {
    boolean stepTicks = cs.stepTicks;
    rnd = new Random();
    
    // Shuffle cars
    for (int i=0; i<cars.length*5; i++) {
      int one = rnd.nextInt(cars.length);
      int two = rnd.nextInt(cars.length);
      String carOne = cars[one];
      cars[one] = cars[two];
      cars[two] = carOne;
    }
    
    Carretera cr = new CarreteraMonitor(4,2);
    
    // Number of cars to simulate
    int numCars = rnd.nextInt(cars.length-1)+1;
    AtomicInteger carsToExit = new AtomicInteger(numCars);
    
    System.out.println("Simulation of "+numCars+" cars moving in a carretera of distance 4 with 2 lanes");
    
    for (int i=0; i<numCars; i++) {
      String car = cars[i];
      int velocidad = velocidades.get(car);
      
      Thread carTh = new Thread(car) {
          public void run() {
            Call call = null;
            Object result = null;
            
            call = Call.enter(car); publish(call); result = cr.enter(car); publish(call.returned(result));
            call = Call.moving(car,velocidad); publish(call); cr.moving(car,velocidad); publish(call.returned());
            call = Call.move(car); publish(call); result = cr.move(car); publish(call.returned(result));
            call = Call.moving(car,velocidad); publish(call); cr.moving(car,velocidad); publish(call.returned());
            call = Call.move(car); publish(call); result = cr.move(car); publish(call.returned(result));
            call = Call.moving(car,velocidad); publish(call); cr.moving(car,velocidad); publish(call.returned());
            call = Call.move(car); publish(call); result = cr.move(car); publish(call.returned(result));
            call = Call.moving(car,velocidad); publish(call); cr.moving(car,velocidad); publish(call.returned());
            call = Call.exit(car); publish(call); cr.exit(car); publish(call.returned());
            carsToExit.decrementAndGet();
          }
        };
      carTh.start();
    }
    
    Thread timeThread = new Thread("tick") {
        public void run() {
          do {
            if (stepTicks) {
              try {
                cs.tickQueue.take();
              } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
            } else {
              
              try {
                Thread.sleep(5000);
              } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
            }
            
            Call call = Call.tick(); publish(call); cr.tick(); publish(call.returned()); 
          } while (carsToExit.get() > 0);
        }
      };	
    timeThread.start();
    return null;
  }
}

class Call {
  String name;
  Object parm1=null;
  Object parm2=null;
  boolean returned;
  Object result=null;
  
  Call() { }
  
  static Call enter(String car) {
    Call call = new Call(); call.name = "enter"; call.parm1 = car; call.returned = false; return call;
  }
  
  static Call move(String car) {
    Call call = new Call(); call.name = "move"; call.parm1 = car; call.returned = false; return call;
  }
  
  static Call exit(String car) {
    Call call = new Call(); call.name = "exit"; call.parm1 = car; call.returned = false; return call;
  }
  
  static Call moving(String car, int velocidad) {
    Call call = new Call(); call.name = "moving"; call.parm1 = car; call.parm2 = velocidad; call.returned = false; return call;
  }
  
  static Call tick() {
    Call call = new Call(); call.name = "tick"; call.returned = false; return call;
  }
  
  public Call returned() {
    Call newCall = new Call(); newCall.name = name; newCall.parm1 = parm1; newCall.parm2 = parm2;
    newCall.returned = true; newCall.result = null; return newCall;
  }
  
  public Call returned(Object result) {
    Call newCall = returned(); newCall.result = result; return newCall;
  }
  
  public String getCallString() {
    String str = name+"(";
    if (parm1 != null) str +=parm1;
    if (parm2 != null) str +=","+parm2;
    str += ")";
    return str;
  }
  
  public String toString() {
    String str = getCallString();
    if (returned) {
      str += " returned";
      if (result != null) str += " "+result;
      str += ";";
    }
    return str;
  }
}



