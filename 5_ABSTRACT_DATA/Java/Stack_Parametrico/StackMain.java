import java.io.*;

public class StackMain {

	public static Integer sum(Integer a, Integer b) {
		Integer s;
		s = a + b;
		return s;
	}

	public static void main(String[] args) {
		Stack<Integer> myStack;
		if (args.length <= 0) return;

		myStack = new Stack<>(100);

		for (int i = 0; i < Integer.parseInt(args[0]); i++) 
			myStack.push(i);

		while (!myStack.empty()) {
			System.out.println("Stack top is: " + myStack.top());
			myStack.pop();
		}

		System.out.println(sum(1, 1));

		return;
	}
}