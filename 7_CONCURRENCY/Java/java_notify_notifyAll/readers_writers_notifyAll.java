public class Buffer {

	private double x, f_x;
	private int nreaders, nwriters;

	public Buffer() {
		nreaders = 0; nwriters = 0;
	}

	public synchronized double start_read() {
		while (nwriters > 0) wait();
		nreaders++;
		return x;
	}
	public synchronized void start_write(double x) {
		while (nreaders > 0 || nwriters > 0) wait();
		nwriters++;
		this.x = x;
	}
	public synchronized void end_read(double f_x) {
		nreaders--;
		notifyAll();
		return f_x;
	}
	public synchronized void end_write(double f_x) {
		nwriters--;
		this.f_x = f_x;
		notifyAll();
	}

}
