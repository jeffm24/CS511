import java.util.concurrent.ArrayBlockingQueue;

/*
 *  The Producer class parses the XML file into Pages and loads
 *  the Pages into the sharedQueue for the Consumer to analyze.
 */
public class Producer implements Runnable
{
    private ArrayBlockingQueue<Page> sharedQueue;
    private Integer numPages;
    private String fileName;

    /*
     *  q - sharedQueue to put Pages into
     *  numP - number of Pages to get from the XML file
     *  file - filepath of the XML input file to analyze
     */
    public Producer(ArrayBlockingQueue<Page> q, Integer numP, String file) {
        sharedQueue = q;
        numPages = numP;
        fileName = file;
    }

    public void run() {
        // parse XML into pages
        Iterable<Page> allPages = new Pages(numPages, fileName);

        // put pages into the sharedQueue
        try {
            for (Page pg: allPages) {
                sharedQueue.put(pg);
            }

            // put PoisonPill object to signal end of pages
            sharedQueue.put(new PoisonPill());
        } catch (InterruptedException e) {}


    }

}
