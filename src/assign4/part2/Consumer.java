import java.util.HashMap;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.locks.ReentrantLock;

/*
 *  The Consumer class takes Pages from the sharedQueue and keeps track
 *  of the number of occurrences of the most frequently used tokens
 *  from all of the Pages.
 */
public class Consumer implements Runnable
{
    private ArrayBlockingQueue<Page> sharedQueue;
    private HashMap<String, Integer> tokenFreq;
    private ReentrantLock hashLock;

    /*
     *  q - sharedQueue to put Pages into
     *  tf - shared HashMap passed from main to store token frequencies in
     *  lock - ReentrantLock for mutex on the shared HashMap
     */
    public Consumer(ArrayBlockingQueue<Page> q, HashMap<String, Integer> tf, ReentrantLock lock) {
        sharedQueue = q;
        tokenFreq = tf;
        hashLock = lock;
    }

    public void run() {
        Page pg = null;
        Iterable<String> allTokens = null;

        try {
            // on each page received from the sharedQueue, find all tokens then increase the count for each token
            while (true) {
                // wait for lock before entering critical section
                hashLock.lock();

                try {
                    pg = sharedQueue.take();

                    // if PoisonPill object is taken from the sharedQueue, release lock, exit the loop, and return
                    if (pg instanceof PoisonPill)
                        break;

                    allTokens = new Words(pg.getText());

                    for (String s: allTokens)
                        countToken(s);
                } finally {
                    //release lock once out of critical section
                    hashLock.unlock();
                }
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
