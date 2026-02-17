class Stack<Type> {

	// Properties
	private Object[] stackStorage;
	private int stackTopPointer;

	// Methods
	
	public Stack(int myMaxStackDim) {
		stackStorage = new Object[myMaxStackDim];
		stackTopPointer = 0;
	}

	public void push(Type elem) {
		if (stackTopPointer < stackStorage.length)
			stackStorage[stackTopPointer++] = elem;
	}

	public void pop() {
		if (stackTopPointer > 0) stackTopPointer--;
	}

	@SuppressWarnings("unchecked")
	public Type top() {
		if (stackTopPointer > 0) 
			return (Type) stackStorage[stackTopPointer - 1];
		else
			return null;
	}

	public boolean empty() {
		return (stackTopPointer == 0);
	}
}