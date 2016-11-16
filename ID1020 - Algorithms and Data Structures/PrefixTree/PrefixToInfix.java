package se.kth.ID1020.PrefixTree;

import java.util.List;

import se.kth.ID1020.NltEngine_ver2.ListOfWords;
import se.kth.id1020.util.Document;
import edu.princeton.cs.algs4.Stack;
import edu.princeton.cs.introcs.StdIn;
import edu.princeton.cs.introcs.StdOut;

public class PrefixToInfix {
	
	
	
    public static String convert(String query) {
    	String[] str = query.split("\\s+");
        Stack<String> stack = new Stack<String>();
        StringBuilder bldr = new StringBuilder();
        for (int i = str.length-1; i>=0; i--) {
            String s = str[i];
            if      (s.equals("+")) stack.push(s);
            else if (s.equals("-")) stack.push(s);
            else if (s.equals("|")) stack.push(s);
            else if (s.equals("(")) StdOut.print(stack.pop() + " ");
            else if (s.equals(")")) StdOut.print("");
            else                    StdOut.print(s + " ");
        }
      return bldr.toString();
    }
}

