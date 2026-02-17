import java.io.*;

public class MyRunnable implements Runnable {

	private int id;

	public MyRunnable( int id ) {
		this.id = id;
	}

	public void run() {
		for (int i = 0; i<50; i++) {
			System.out.println("I'm implementing Runnable");
			try {
				Thread.sleep( id * 100 );
			} catch (InterruptedException e) {
				System.out.println("Interrupted");
			}
		}
	}

}

