import java.io.*;

public class MyThread extends Thread {

	private int id;

	public MyThread( int id ) {
		this.id = id;
	}

	public void run() {
		for (int i = 0; i<50; i++) {
			System.out.println("I'm extending Thread");
			try {
				Thread.sleep( id * 100 ); // Thread. is redundant
			} catch (InterruptedException e) {
				System.out.println("Interrupted");
			}
		}
	}
}
