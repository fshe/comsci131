/* Name: Frank She

   UID: 204172020

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*/
import java.lang.*;
import java.util.concurrent.*;
import java.util.Arrays;
import java.io.*;
import java.util.stream.*;

//Consider: using the Math library
// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
    	R = r;
		G = g;
		B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }
}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname) 
    	throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
	    	int b = is.read();
	    	if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
	    	int offset = i * 3;
	    	pixels[i] = new RGB(bytes[offset] & MASK, 
	    						bytes[offset+1] & MASK, 
	    						bytes[offset+2] & MASK);
		}
		is.close();

		this.width = width;
		this.height = height;
		this.maxColorVal = max;
		this.pixels = pixels;
    }

	// write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n" 
						+ maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;

		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
	    	bytes[i] = (byte) rgb.R;
	    	bytes[i+1] = (byte) rgb.G;
	    	bytes[i+2] = (byte) rgb.B;
	    	i += 3;
		}
		os.write(bytes);
		os.close();
    }

	// implement using Java 8 Streams
    public PPMImage negate() {
    	RGB[] out =
    		Arrays.stream(pixels.clone())
    			.parallel()
    			.map( (pixel) -> new RGB(maxColorVal-pixel.R, maxColorVal-pixel.G, maxColorVal-pixel.B) )
    			.toArray(RGB[]::new);
    	return new PPMImage(width, height, maxColorVal, out);
    }

	// implement using Java 8 Streams
    public PPMImage greyscale() {
    	RGB[] out =
    		Arrays.stream(pixels.clone())
    			.parallel()
    			.map( 
    				(pixel) -> 
    					new RGB(
    					(int) Math.round(0.299*pixel.R + 0.587*pixel.G + 0.114*pixel.B), 
    					(int) Math.round(0.299*pixel.R + 0.587*pixel.G + 0.114*pixel.B), 
    					(int) Math.round(0.299*pixel.R + 0.587*pixel.G + 0.114*pixel.B)
    					)
    				)
    			.toArray(RGB[]::new);
    	return new PPMImage(width, height, maxColorVal, out);
    }    
    
	// implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
		class MirrorTask extends RecursiveTask<RGB[]> 
		{
			private final int SEQUENTIAL_CUTOFF = 100000; //dragon: testing purely sequential
    		private RGB[] arr;
			private int low, hi, width;

			public MirrorTask(RGB[] a, int low, int hi, int width) {
				this.arr = a;
				this.low = low;
				this.hi = hi;
				this.width = width;
			}

			public RGB[] compute() {
				if(hi - low > SEQUENTIAL_CUTOFF) {
					int mid = (low+hi)/2;
					MirrorTask left = new MirrorTask(arr, low, mid, width);
					MirrorTask right = new MirrorTask(arr, mid, hi, width);
					left.fork();
					RGB[] l2 = right.compute();
					RGB[] l1 = left.join();
					//combine the arrays
					RGB[] combined = new RGB[l2.length + l1.length];
					for(int i = 0; i < l2.length + l1.length; i++)
					{
						if( i < l1.length)
							combined[i] = l1[i];
						else
							combined[i] = l2[i-l1.length];
					}
					return combined;
				} 
				else { //base case, just do the sequential change
					RGB[] switched = new RGB[hi-low];
					for (int i = low; i < hi; i++)
					{
						int index = (i+width) - (2 * (i % width) + 1);
						switched[i-low] = arr[index];
					}
					return switched;
				}
			}
		}
		RGB[] out = new MirrorTask(pixels, 0, width*height, width).compute();
    	return new PPMImage(width, height, maxColorVal, out);
	}

    //mirror image: replace the current pixel with the pixel of the same height, but width-it's width
	// implement using Java 8 Streams
    public PPMImage mirrorImage2() {
    	RGB[] out =
    		IntStream.range(0, width*height)
    			.parallel()
    			.mapToObj( (i) -> new RGB(pixels[(i+width) - (2 * (i % width) + 1)].R, pixels[(i+width) - (2 * (i % width) + 1)].G, pixels[(i+width) - (2 * (i % width) + 1)].B) )
    			.toArray(RGB[]::new);
    	return new PPMImage(width, height, maxColorVal, out);
    }

	// implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {

		class GaussianTask extends RecursiveTask<RGB[]> 
		{
			private final int SEQUENTIAL_CUTOFF = 100;
    		private RGB[] arr;
			private int low, hi, width, total;
			private double[][] filter;

			public GaussianTask(RGB[] a, int low, int hi, int width, int total, double[][] filter) {
				this.arr = a;
				this.low = low;
				this.hi = hi;
				this.width = width;
				this.filter = filter;
				this.total = total;
			}

			public RGB[] compute() {
				if(hi - low > SEQUENTIAL_CUTOFF) {
					int mid = (low+hi)/2;
					GaussianTask left = new GaussianTask(arr, low, mid, width, total, filter);
					GaussianTask right = new GaussianTask(arr, mid, hi, width, total, filter);
					left.fork();
					RGB[] l2 = right.compute();
					RGB[] l1 = left.join();
					//combine the arrays
					RGB[] combined = new RGB[l2.length + l1.length];
					for(int i = 0; i < l2.length + l1.length; i++)
					{
						if( i < l1.length)
							combined[i] = l1[i];
						else
							combined[i] = l2[i-l1.length];
					}
					return combined;
				} 

				else { //base case, just do the sequential work
					RGB[] switched = new RGB[hi-low];
					int pos = filter.length/2;
					for (int i = low; i < hi; i++) //for each pixel
					{
						double sumR = 0;
						double sumG = 0;
						double sumB = 0;

						int index = i % width; //get the position

						for (int j = 0; j < filter.length; j++) {
							int position = i; //the position to use
							//adjust position based on the row
							for(int j2 = 0, compare = i; j2 < Math.abs(pos-j); j2++)
							{
								if(j < pos && compare > width) //move up
								{
									position -= width;
									compare -= width;
								}
								else if (j > pos && compare < total-width) //move down
								{
									position += width;
									compare += width;
								}
							}

							for(int k = 0; k < filter.length; k++)
							{
								for(int k2 = 0, compare = index; k2 < Math.abs(pos-k); k2++) //right and left
								{
									if(k < pos && compare > 0) //move left
									{
										position -= 1;
										compare -= 1;
									}
									else if (k > pos && compare < width-1) //move right
									{
										position += 1;
										compare += 1;
									}
								}
								sumR += filter[j][k] * arr[position].R;
								sumG += filter[j][k] * arr[position].G;
								sumB += filter[j][k] * arr[position].B;
							}
						}
						switched[i-low] = new RGB( (int) Math.round(sumR), (int) Math.round(sumG), (int) Math.round(sumB));
					}
					return switched;
				}
			}
		}
		double[][] filter = Gaussian.gaussianFilter(radius, sigma);
		RGB[] out = new GaussianTask(pixels, 0, width*height, width, width*height, filter).compute();
    	return new PPMImage(width, height, maxColorVal, out);
	}
}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
	    	hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
	    	}
		}
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
    }
}
/*
class Test {
    public static void main(String[] args) {
    	try{
			PPMImage original = new PPMImage("florence.ppm");
			PPMImage product = original.greyscale();
			product.toFile("mirror1.ppm");
			product = original.gaussianBlur(1,2);
						product.toFile("mirror2.ppm");

		} catch (FileNotFoundException e)
		{
			System.out.print("File Not Found");
		} catch (IOException e) { System.out.print("IO EXCEPTION"); }
	}
}

class Test2 {
	public static void main(String[] args) {
		RGB[] array = { new RGB(0,0,0), new RGB(1,1,1), new RGB(2,2,2), new RGB(3,3,3)};
		PPMImage original = new PPMImage(2,2,100, array);
		PPMImage product = original.negate();
		assert( product.pixels[0].R == 100 && product.pixels[1].R == 99 && product.pixels[3].G == 97); //dragon change pixels back to the protected

		product = original.greyscale();
		assert( product.pixels[0].R == 0 && product.pixels[1].R == Math.round(.299+.587+.114) && product.pixels[3].G == Math.round(.299*3+.587*3+.114*3) ); //dragon change pixels back to the protected

		product = original.mirrorImage();
		assert( product.pixels[0].R == 1 && product.pixels[1].R == 0 && product.pixels[3].G == 2 ); //dragon change pixels back to the protected
		
		RGB[] array2 = {new RGB(0,0,0), new RGB(1,1,1), new RGB(2,2,2), new RGB(3,3,3), new RGB(4,4,4), 
			new RGB(10,10,10), new RGB(11,11,11), new RGB(12,12,12), new RGB(13,13,13), new RGB(14,14,14) }; 
		PPMImage original2 = new PPMImage(5,2, 15, array2);

		PPMImage product2 = original2.mirrorImage2(); 
		assert( product2.pixels[0].R == 4 && product2.pixels[1].R == 3 && product2.pixels[2].G == 2 && product2.pixels[3].G == 1 && product2.pixels[4].G == 0
				&& product2.pixels[5].R == 14 && product2.pixels[6].R == 13 && product2.pixels[7].R == 12 && product2.pixels[8].R == 11 && product2.pixels[9].R == 10
			); //dragon change pixels back to the protected
		double[][] filter = Gaussian.gaussianFilter(1, 0.2);
		for(int i = 0; i < filter.length; i++)
			for (int j = 0; j <filter[i].length; j++)
			{
				System.out.print(filter[i][j]);
				System.out.print("\n");
			}
			
		product = original.gaussianBlur(1, 0.2);

		System.out.print(product.pixels[0].R);
			System.out.print("\n");
		System.out.print(Math.round( filter[0][0]* (0) + filter[0][1]* (0) + filter[0][2]* (1) +
			filter[1][0]* (0) + filter[1][1]* (0) + filter[1][2]* (1)+
			filter[2][0]* (0) + filter[2][1]* (2) + filter[2][2]* (3) ));
		System.out.print("\n");

		assert( product.pixels[0].R == Math.round(
			filter[0][0]* (0) + filter[0][1]* (0) + filter[0][2]* (1) +
			filter[1][0]* (0) + filter[1][1]* (0) + filter[1][2]* (1)+
			filter[2][0]* (0) + filter[2][1]* (2) + filter[2][2]* (3) 
			));

		//	);


	}
}
*/
