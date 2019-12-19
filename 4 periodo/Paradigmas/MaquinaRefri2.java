import java.util.concurrent.Semaphore;

public class MaquinaRefri2 {
	int latas;
	int capacidade;
	
	public MaquinaRefri2() {
		super();
		this.latas = 0;
		this.capacidade = 100;
	}

	public int getLatas() {
		return latas;
	}

	public void setLatas(int latas) {
		this.latas = latas;
	}
	
	public int getCapacidade() {
		return capacidade;
	}
	
	private static void nap(int millisecs) {
        try {
            Thread.sleep(millisecs);
        } catch (InterruptedException e) {
            System.err.println(e.getMessage());
        }
    }

	static Semaphore lock = new Semaphore(1, true);
    
    private static void addLata(MaquinaRefri2 maq, int latas) throws InterruptedException {
    	
    	while(true) {
    		lock.acquire();
    		if((maq.getLatas() + latas) < maq.getCapacidade()) {
        		maq.setLatas(maq.getLatas() + latas);
        		System.out.println("Adicionei " + latas +" latas! (Latas na maquina: " + maq.getLatas() + ").");
    		}
    		else
    		{
    			
    			if(100 - maq.getLatas() != 0)
    			{
    				System.out.println("Adicionei " + (100 - maq.getLatas()) +" latas! (Latas na maquina: " + maq.getLatas() + ").");
    			}
    			else
    			{
    				System.out.println("Maquina cheia, não posso colocar.");
    			}
    			maq.setLatas(100);
    		}
    		lock.release();
    		nap(6000);
    	}
    	
    }
    
    private static void consomeLata(MaquinaRefri2 maq) throws InterruptedException {
    	
    	nap(1000);
    	
    	while(true) {
    		if(maq.getLatas() > 0) {
    			lock.acquire();
            	maq.setLatas(maq.getLatas() - 1);
            	System.out.println("Consumi uma lata! (Latas na maquina: " + maq.getLatas() + ").");
            	lock.release();
    		}
        	nap(500);
    	}
    	
    }
	
	public static void main(String [] args) {
		
		MaquinaRefri2 maq2 = new MaquinaRefri2();
		
		//Adds five cans.
		new Thread () {
		    public void run() {
			try {
				addLata(maq2, 5);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();
		
		//Adds twenty-five cans.
		new Thread () {
		    public void run() {
			try {
				addLata(maq2, 25);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();
		
		//Adds three cans.
		new Thread () {
		    public void run() {
			try {
				addLata(maq2, 3);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();
		
		//Adds ten cans.
		new Thread () {
		    public void run() {
			try {
				addLata(maq2, 10);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();
		
		
		//Consumer threads
		new Thread () {
		    public void run() {
			try {
				consomeLata(maq2);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();
		
		new Thread () {
		    public void run() {
			try {
				consomeLata(maq2);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    }
		}.start();

	    }
}