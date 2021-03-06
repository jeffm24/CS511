import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

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
        ConcurrentHashMap<String, Integer> tokenFreq = new ConcurrentHashMap<String, Integer>();

        // begin timed code ...
    	final long before = System.nanoTime();

        int numProc = Runtime.getRuntime().availableProcessors();

        // create producer thread
        Thread producer = new Thread(new Producer(sharedQueue, numPages, fileName, numProc - 1));
        producer.start();

        // create consumer threads
        int createdThreads;

        ExecutorService executor = Executors.newFixedThreadPool(numProc - 1);
        for (createdThreads = 0 ; createdThreads < numProc - 1 ; createdThreads++)
            executor.execute(new Thread(new Consumer(sharedQueue, tokenFreq)));

        // print number of available processors
    	System.out.println(numProc + " available processors (created " + createdThreads + " Consumer theads)");

        // wait for threads to finish
        try {
            producer.join();
            executor.shutdown();
            while (!executor.isTerminated())
                ;
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
