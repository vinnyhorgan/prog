class IntStack {

	// Properties
	private int[] stackStorage;
	private int stackTopPointer;

	// Methods
	
	public IntStack(int myMaxStackDim) {

		stackStorage = new int [myMaxStackDim];
		stackTopPointer = 0;

	}

	public void push(int elem) {
		if (stackTopPointer < stackStorage.length)
			stackStorage[ stackTopPointer++ ] = elem;
	}

	public void pop() {
		if (stackTopPointer > 0) stackTopPointer--;
	}

	public int top() {
		if (stackTopPointer > 0) 
			return stackStorage[ stackTopPointer -1 ];
		else
			return -1;
	}

	public boolean empty() {
		return (stackTopPointer == 0);
	}
}
