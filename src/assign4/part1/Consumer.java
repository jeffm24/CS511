import java.util.HashMap;
import java.util.concurrent.ArrayBlockingQueue;

/*
 *  The Consumer class takes Pages from the sharedQueue and keeps track
 *  of the number of occurrences of the most frequently used tokens
 *  from all of the Pages.
 */
public class Consumer implements Runnable
{
    private ArrayBlockingQueue<Page> sharedQueue;
    private HashMap<String, Integer> tokenFreq;

    /*
     *  q - sharedQueue to put Pages into
     *  tf - HashMap passes from main to store token frequencies in
     */
    public Consumer(ArrayBlockingQueue<Page> q, HashMap<String, Integer> tf) {
        sharedQueue = q;
        tokenFreq = tf;
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
        } catch (InterruptedException e) {}
    }

    private void countToken(String tok) {
    	Integer currentCount = tokenFreq.get(tok);
    	if (currentCount == null)
    	    tokenFreq.put(tok, 1);
    	else
    	    tokenFreq.put(tok, currentCount + 1);
    }
}
