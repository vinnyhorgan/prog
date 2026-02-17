import java.io.*;

public class StackMain {

	public static void main(String[] args) {
	IntStack myStack;

		if (args.length <= 0) return;

		myStack = new IntStack(100);

		for (int i = 0; i<Integer.parseInt(args[0]); i++) myStack.push(i);

		while ( ! myStack.empty() ) {
			System.out.println("Stack top is:" +  myStack.top() );
			myStack.pop();
		}

		return;
		
	}
}

