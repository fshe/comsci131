/* Name: Frank She

   UID: 204172020

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }

    public Num (double value) { val = value; }

    public double eval() { return val; }

    public List<Instr> compile() {
        List<Instr> list = new LinkedList<Instr>();
        list.add(new Push(val));
        return list;
    }
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }

    public BinOp (Exp e1, Op o, Exp e2) {
        left = e1;
        right = e2;
        op = o;
    }
    public double eval() {
        return op.calculate(left.eval(), right.eval());
    }

    public List<Instr> compile() {
        List<Instr> list = new LinkedList<Instr>();
        list.addAll(left.compile());
        list.addAll(right.compile());  //check this order
        list.add( new Calculate(op) );

        return list;
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for arithmetic instructions
interface Instr {
    void executeOne(Stack<Double> stack);
}

class Push implements Instr {
    protected double val;

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }
    public Push (double value) { val = value; }

    public void executeOne(Stack<Double> stack) { 
        stack.push(val); 
    }
}

class Calculate implements Instr {
    protected Op op;

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }    

    public Calculate (Op operation) {
        op = operation;
    }

    public void executeOne(Stack<Double> stack) { 
        double two = stack.pop();
        double one = stack.pop();
        stack.push(op.calculate(one, two)); 
    }
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    public double execute() {
        Stack<Double> stack = new Stack<Double>();
        for(int i = 0; i < instrs.size(); i++)
            instrs.get(i).executeOne(stack);
        return stack.pop();
    }  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
	     // a test for Problem 1a   (1+2) * 3
		 Exp exp =
	     	new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
		     	  	  Op.TIMES,
		       	  new Num(3.0));
		 assert(exp.eval() == 9.0);

         // 3-2
         Exp exp2 = 
            new BinOp(new Num(3.0), Op.MINUS, new Num(2.0));
        assert(exp2.eval() == 1.0);

        // (1+2) * (6/2)   == 9
        Exp exp3 = new BinOp(
            new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)), 
            Op.TIMES,
            new BinOp(new Num(6.0), Op.DIVIDE, new Num(2.0)) 
            );

        assert(exp3.eval() == 9.0);

		// // a test for Problem 1b
		 List<Instr> is = new LinkedList<Instr>();
		 is.add(new Push(1.0));
		 is.add(new Push(2.0));
		 is.add(new Calculate(Op.PLUS));
		 is.add(new Push(3.0));
		 is.add(new Calculate(Op.TIMES));
		 Instrs instrs = new Instrs(is);
		 assert(instrs.execute() == 9.0);

		// // a test for Problem 1c
		 assert(exp.compile().equals(is));

        List<Instr> is2 = new LinkedList<Instr>();
         is2.add(new Push(1.0));
         is2.add(new Push(2.0));
         is2.add(new Calculate(Op.PLUS));
         is2.add(new Push(6.0));
         is2.add(new Push(2.0));
         is2.add(new Calculate(Op.DIVIDE));
         is2.add(new Calculate(Op.TIMES));
         Instrs instrs2 = new Instrs(is2);
         assert(instrs2.execute() == 9.0);
        
         assert(exp3.compile().equals(is2));

    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
     int size();
     boolean contains(String s);
     void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    protected SNode head; //represents the head of the list. SNode is either an SEmpty Node which is the end of the list or an SElement Node which is a node that contains an element of the set

    public ListStringSet() {
        head = new SEmpty();
    }

    public int size() {
        return head.size();
    }

    public boolean contains(String s){ 
        return head.contains(s);
    }

    public void add(String s){ 
        if(!head.contains(s))
            head = head.add(s);
    }
    public void printList()
    {
        head.print();
    }

}

// a type for the nodes of the linked list
interface SNode { //add methods to this and to SEmpty and SElement
    int size();
    boolean contains(String s);
    SNode add(String s);
    void print();
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    public int size() { return 0; }
    public boolean contains(String s) { return false; }
    public SNode add(String s) { return new SElement(s); }
    public void print() { System.out.print("EMPTY\n\n"); }
}

// represents a non-empty node
class SElement implements SNode {

    protected String elem;
    protected SNode next;

    public SElement (String s) {
        elem = s;
        next = new SEmpty();
    }

    public int size() { return 1 + next.size(); }

    public boolean contains(String s)
    {
        if(elem.compareTo(s) < 0)
            return next.contains(s);
        else if (elem.compareTo(s) == 0)
            return true;
        else
            return false;
    }

    public SNode add(String s) {
        if(s.compareTo(elem) < 0) //s is lexicographically less than elem
        {
            SElement returnVal = new SElement(s);
            returnVal.next = this;
            return returnVal;
        }
        else
        {
            this.next = next.add(s);
            return this;
        }
    }

    public void print() { 
        System.out.print(elem + "\n"); 
        next.print();
    }

}

// PROBLEM 2B
interface Set<T> {
    int size();
    boolean contains(T s);
    void add(T s);
}

class ListSet<T> implements Set<T> {
    protected Node<T> head;
    protected Comparator<T> comparator;

    ListSet(Comparator<T> c) {
        head = new Empty<T>();
        comparator = c;
    }

    public int size() {
        return head.size();
    }

    public boolean contains(T s){ 
        return head.contains(s, comparator);
    }

    public void add(T s){ 
        if(!head.contains(s, comparator))
            head = head.add(s, comparator);
    }

    public void printList()
    {
        head.print();
    }
}

interface Node<T> {
    int size();
    boolean contains(T s, Comparator<T> c);
    Node<T> add(T s, Comparator<T> c);
    void print();
}

class Empty<T> implements Node<T> {
    public int size() { return 0; }
    public boolean contains(T s, Comparator<T> c) { return false; }
    public Node<T> add(T s, Comparator<T> c) { return new Element<T>(s); }
    public void print() { System.out.print("EMPTY\n\n"); }
}

class Element<T> implements Node<T> {
    
    protected T elem;
    protected Node<T> next;

    public Element (T s) {
        elem = s;
        next = new Empty<T>();
    }

    public int size() { return 1 + next.size(); }

    public boolean contains(T s, Comparator<T> c)
    {
        if(c.compare(elem, s) == 0)
            return true;
        else
          return next.contains(s, c);
    }

    public Node<T> add(T s, Comparator<T> c) {
        if(c.compare(s, elem) < 0)
        {
            Element<T> returnVal = new Element<T>(s);
            returnVal.next = this; //s should ne less than em
            return returnVal;
        }
        else
        {
            this.next = next.add(s, c);
            return this;
        }
    }

    public void print() { 
        System.out.print(elem + "\n"); 
        next.print();
    }
}

/*
//Test Cases for Problem 2
class Part2Test {
    public static void main(String[] args) {
        // a test for Problem 2a
        ListStringSet lss = new ListStringSet();
        assert(lss.size() == 0);
        lss.add("Frank");
        lss.add("Frank");
        assert(lss.size() == 1);
        lss.add("Red");
        lss.add("Awkward");
        assert(lss.size() == 3);
        lss.add("Anthony");
        lss.add("Aaron");
        assert(lss.size() == 5);
        lss.add("Aaron");
        lss.add("Aaron");

        lss.printList();
        //a test for Problem 2b
        System.out.print("All tests succeeded\n");

    }
}

class Part2BTest {
    public static void main(String[] args) {
        // a test for Problem 2a
        ListSet<Integer> lss = new ListSet<Integer>((Integer s1, Integer s2) -> 
        
            s2.compareTo(s1)

        

            );
        assert(lss.size() == 0);
        lss.add(1);
        lss.add(1);
        assert(lss.size() == 1);
        lss.add(5);
        lss.add(2);
        assert(lss.size() == 3);
        lss.add(6);
        lss.add(12);
        assert(lss.size() == 5);
        lss.add(1);
        lss.add(5);

        lss.printList();
         ListSet<Integer> sList = new ListSet<Integer>((Integer s1, Integer s2) -> s2 - s1);
         sList.add(1);
         sList.add(3);
         sList.add(-6);
         sList.add(3);
         sList.add(100);
         sList.printList();
        //a test for Problem 2b
        System.out.print("All tests succeeded\n");

    }
}
*/





