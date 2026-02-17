import java.io.*;

public class Buffer {

	private int in;

	public void Buffer () {
		in = 0;
	}

	public synchronized void access (Thread me) {

		try {
			if (in > 0) wait();
		} catch (InterruptedException e) {}
		in++;

		System.out.println("I'm in");
		try {
			me.sleep(1000);
		} catch (InterruptedException e) {}
		//for (int i = 0; i<1000000; i++);

	
		System.out.println("I'm about to exit");

		in--;
		notify();

	}
}
