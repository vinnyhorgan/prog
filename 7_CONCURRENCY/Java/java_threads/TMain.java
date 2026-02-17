import java.io.*;

public class TMain {
	
	public static void main(String[] argv) {
	MyThread myThread = new MyThread(1);
	Runnable myRunnable = new MyRunnable(2);

	Thread myRunnableThread = new Thread(myRunnable);

		myThread.start();
		myRunnableThread.start();
	}

}
