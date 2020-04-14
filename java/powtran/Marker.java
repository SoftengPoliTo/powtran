package powtran;

import java.util.Arrays;

public class Marker {

	final static int N_THREADS = Runtime.getRuntime().availableProcessors();
	private static void generateMarker(long markerLength) throws InterruptedException {
		Thread[] ts = new Thread[N_THREADS];

		Thread.sleep(markerLength);//SLEEP

		final long temp=System.currentTimeMillis()+markerLength; // end time for BUSY partle
		Runnable busy = ()->{
			int count=0;
			while(temp>System.currentTimeMillis()){//BUSY
				for(int i=0; i<markerLength;++i){count+=i;}
			}
		};

		Arrays.setAll(ts, t -> { // BUSY
			Thread m = new Thread(busy,"Powtran marker " + t);
			m.start();
			return m;
		});

		for(int t=0; t<ts.length; ++t) { // wait for all busy threads to complete
			ts[t].join();
		}

		Thread.sleep(markerLength); // SLEEP
	}

}
