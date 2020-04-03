package cc.carretera;
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

import java.time.Duration;

public class Tests {
	@Test 
	public void test_01() {
		Carretera cr = new CarreteraMonitor(3,2);
		assertTimeout(Duration.ofMillis(10000), () -> {
		Position p0 = cr.enter("seat");
		System.out.println("after enter");
		Assertions.assertEquals(p0.getX(),0);
		Assertions.assertTrue((p0.getY() == 0) || (p0.getY() == 1));
		Thread t1 = new Thread(){
		    public void run() {
		    	System.out.println("before sleeping");
		    	try {
					Thread.sleep(10);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		    	System.out.println("after sleeping"); System.out.flush();
		    	System.out.println("tick");
		    	cr.tick();
		    	System.out.println("tick");
				cr.tick();
		    	System.out.println("tick");
				cr.tick();
		    	System.out.println("tick");
				cr.tick();
		    	System.out.println("tick");
		    }
	 };
	 Thread t2 = new Thread() {
		 public void run() {;
		 System.out.println("beginning moving");
			 cr.moving("seat", 3);
				System.out.println("after moving");
				cr.move("seat");
				System.out.println("after move");
		 }
	 };
	 
	t2.start();
	t1.start();
		});
	}
}
