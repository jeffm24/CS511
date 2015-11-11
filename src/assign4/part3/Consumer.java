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

    /*
     *  q - sharedQueue to put Pages into
     *  tf - shared HashMap passed from main to store token frequencies in
     */
    public Consumer(ArrayBlockingQueue<Page> q, ConcurrentHashMap<String, Integer> tf) {
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
        Integer prevVal, newVal, currentCount;

        // if there is no current count nor any previous count for tok, then return (after initializing the count to 1 for tok)
        if ((currentCount = tokenFreq.get(tok)) == null && (prevVal = tokenFreq.put(tok, 1)) == null) {
            return;
        }

        /*
         * Keep getting current value, incrementing it (if not null), and replacing until successful return from replace
         * Reason for this is that the value associated with the key may have changed before the while condition is reached,
         * which would cause replace() to return false.
         */
        do {
            prevVal = tokenFreq.get(tok);
            if (prevVal == null)
                newVal = 1;
            else
                newVal = prevVal + 1;
        } while (!tokenFreq.replace(tok, prevVal, newVal));
    }
}
