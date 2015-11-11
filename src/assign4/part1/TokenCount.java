import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.ArrayBlockingQueue;

/*
 *  Author: Jeff Mariconda
 *  Class: CS-511
 *
 *  Program that uses concurrent threads to separate an input XML file into pages
 *  and then count the number of occurrences of the most frequently used tokens
 *  in that file.
 */
public class TokenCount {

    public static void main(String[] args) throws Exception {

        // check for proper number of command line args
        if (args.length != 2) {
    	    System.out.println("usage: java TokenCount number-of-pages XML-file");
    	    System.exit(0);
    	}

        // get command line arguments
    	Integer numPages = Integer.parseInt(args[0]);
        String fileName = args[1];

        final int queueLength = 100;

        // create shared queue and hashmap
        ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(queueLength);
        HashMap<String, Integer> tokenFreq = new HashMap<String, Integer>();

        // begin timed code ...
    	final long before = System.nanoTime();

        // create producer thread
        Thread producer = new Thread(new Producer(sharedQueue, numPages, fileName));
        producer.start();

        // create consumer thread
        Thread consumer = new Thread(new Consumer(sharedQueue, tokenFreq));
        consumer.start();

        // print number of available processors
    	System.out.println(Runtime.getRuntime().availableProcessors() + " available processors");

        // wait for threads to finish
        try {
            producer.join();
            consumer.join();
        } catch (InterruptedException ex) {}

        final long after = System.nanoTime();
        // ... end  timed code

    	System.out.println("Time to process " + numPages + " pages = " + (after - before)/1000000 + " milliseconds");

        // sort tokenFreq by value & print top 30 most common tokens
    	Set<Entry<String, Integer>> entries = tokenFreq.entrySet();
        ArrayList<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(entries);
        Collections.sort(list, new Comparator<Map.Entry<String, Integer>>()
        {
            public int compare(Map.Entry<String, Integer> obj1, Map.Entry<String, Integer> obj2)
            {
                return (obj2.getValue()).compareTo(obj1.getValue());
            }
        });
        for (int i = 0 ; i < 30; i++)
            System.out.println(list.get(i).getKey() + " appears " + list.get(i).getValue() + " times");

    }


}
