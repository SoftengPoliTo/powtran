package powtran;

import java.util.Arrays;

public class Marker {

	final static int N_THREADS = Runtime.getRuntime().availableProcessors();
	public static void generateMarker(long markerLength) throws InterruptedException {
	  //SLEEP
	  Thread.sleep(markerLength);
	  // BUSY
	  final long endBusy = System.currentTimeMillis() + markerLength;
	  final Thread[] ts = new Thread[N_THREADS];
	  Runnable busy = ()->{ // Busy code
		    while(endBusy>System.currentTimeMillis()){
		      for(int i=0; i<markerLength;++i){ }
		    }};
	  Arrays.setAll(ts, t -> new Thread(busy,"PowTrAn"+t)); // create busy threads
	  for(Thread t : ts) t.start(); // busy threads start
	  for(Thread t : ts) t.join();  // wait for all busy threads
	  // SLEEP
	  Thread.sleep(markerLength);
	}

}
