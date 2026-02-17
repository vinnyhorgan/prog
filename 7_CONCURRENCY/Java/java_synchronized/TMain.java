import java.io.*;

public class TMain {
	
	public static void main(String[] argv) {
	Thread myThread = new Thread();
	Thread mySecThread = new Thread();
	Buffer myBuffer = new Buffer();

		myThread.start();
		mySecThread.start();

		myBuffer.access(myThread);
		myBuffer.access(mySecThread);
	}

}
