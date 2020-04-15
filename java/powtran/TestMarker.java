package powtran;

import static org.junit.Assert.*;

import java.lang.management.ManagementFactory;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.junit.Test;


public class TestMarker {

	@Test
	public void test() throws Exception {
		int markerLength = 2000;

		long t0 = System.currentTimeMillis();
		Thread t = new Thread(() -> {
			try {
				Marker.generateMarker(markerLength);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		});
		t.start();
		getCpuLoad();
		Thread.sleep(markerLength/2); // wakes up in the middlle of first SLEEP
		double sleep = getCpuLoad();  // first read is always 0.0
		System.out.println("Sleep " + sleep + " @ "+ (System.currentTimeMillis()-t0));

		Thread.sleep(markerLength); // wakes up in the middlle of BUSY
		double busy = getCpuLoad();
		System.out.println("Busy " + busy + " @ "+ (System.currentTimeMillis()-t0));
		

		t.join();

		assertTrue(busy > 0.45); // 98% usage as reported by top corresponds to 0.5 returned by getCpuLoad()

		double ratio = busy / sleep;
		
		assertTrue(ratio > 5.0);
	}
	
	
	/**
	 * Compute the CPU load 
	 * 
	 * The value is just an indication, e.g. a 98% usage as reported by
	 * top corresponds to 0.5 returned by getCpuLoad().
	 * 
	 * Probably it is due to the fact the the number or reported processors
	 * is double the number of actual CPU cores (e.g. 12 reported for a 6-code i9)
	 * 
	 * Source: https://stackoverflow.com/a/21962037/3687824
	 * 
	 * @return percentage of CPU load
	 * @throws Exception
	 */
	private static double getCpuLoad() throws Exception {

	    MBeanServer mbs    = ManagementFactory.getPlatformMBeanServer();
	    ObjectName name    = ObjectName.getInstance("java.lang:type=OperatingSystem");
	    //AttributeList list = mbs.getAttributes(name, new String[]{ "ProcessCpuLoad" });
	    AttributeList list = mbs.getAttributes(name, new String[]{ "SystemCpuLoad" });

	    if (list.isEmpty())     return Double.NaN;

	    Attribute att = (Attribute)list.get(0);
	    Double value  = (Double)att.getValue();

	    // usually takes a couple of seconds before we get real values
	    if (value == -1.0)      return Double.NaN;
	    // returns a percentage value with 2 decimal point precision
	    //return ((int)(value * 10000) / 100.0);
	    return value;
	}

}
