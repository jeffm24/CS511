import java.util.HashMap;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

/*
 *  The Consumer class takes Pages from the sharedQueue and keeps track
 *  of the number of occurrences of the most frequently used tokens
 *  from all of the Pages.
 */
public class Consumer implements Runnable
{
    private ArrayBlockingQueue<Page> sharedQueue;
    private ConcurrentHashMap<String, Integer> tokenFreq;
    private ConcurrentHashMap<String, Integer> privateTokenFreq;

    /*
     *  q - sharedQueue to put Pages into
     *  tf - shared HashMap passed from main to store token frequencies in
     */
    public Consumer(ArrayBlockingQueue<Page> q, ConcurrentHashMap<String, Integer> tf) {
        sharedQueue = q;
        tokenFreq = tf;
        privateTokenFreq = new ConcurrentHashMap<String, Integer>();
    }

    public void run() {
        Page pg = null;
        Iterable<String> allTokens = null;

        try {
            // on each page received from the sharedQueue, find all tokens then increase the count for each token
            while (true) {
                pg = sharedQueue.take();

                // if PoisonPill object is taken from the sharedQueue, exit the loop and return
                if (pg instanceof PoisonPill)
                    break;

                allTokens = new Words(pg.getText());

                for (String s: allTokens)
                    countToken(s);
            }

            // merge counts from private frequency map with the shared map
            mergeMaps();
        } catch (InterruptedException e) {}
    }

    private void countToken(String tok) {
    	Integer currentCount = privateTokenFreq.get(tok);
    	if (currentCount == null)
    	    privateTokenFreq.put(tok, 1);
    	else
    	    privateTokenFreq.put(tok, currentCount + 1);
    }

    /*
     *  Merges the private token count hashmap with the shared one.
     */
    private void mergeMaps() {

        for (String tok : privateTokenFreq.keySet()) {

            Integer privateCount = privateTokenFreq.get(tok);
            Integer currentCount = tokenFreq.get(tok);

        	if (currentCount == null)
        	    tokenFreq.put(tok, privateCount);
        	else
        	    tokenFreq.put(tok, currentCount + privateCount);
        }
    }
}
