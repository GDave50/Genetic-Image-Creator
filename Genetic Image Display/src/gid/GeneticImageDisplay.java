package gid;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

import javax.swing.SwingUtilities;

/**
 * This program displays an image created by the Genetic
 * Image program made by Gage Davidson in Lisp.
 * 
 * @author Gage Davidson
 */
class GeneticImageDisplay {
	
	// text file holding image pixels
	private static final String FILEPATH =
			"C:/Users/Gage/Documents/Programming/Eclipse Workspace/Genetic Image Display/img.txt";
	private static final File FILE = new File(FILEPATH);
	
	// range of values a pixel can be
	private static int pixelRange;
	
	/**
	 * @param unusedArgs command line arguments (unused)
	 */
	public static void main(String[] unusedArgs) {
		ArrayList<String> pixelLists = readPixelLists();
		int[][] pixels = makePixelArray(pixelLists);
		
		ImageDisplay display = new ImageDisplay(pixelRange, pixels);
		
		SwingUtilities.invokeLater(() -> display.repaint());
	}
	
	/**
	 * Reads the image pixel file.
	 * @return 2D pixel ArrayList
	 */
	private static ArrayList<String> readPixelLists() {
		ArrayList<String> pixelLists = new ArrayList<>(16);
		
		try (Scanner scan = new Scanner(FILE)) {
			pixelRange = Integer.parseInt(scan.nextLine());
			
			while (scan.hasNextLine())
				pixelLists.add(scan.nextLine());
		} catch (FileNotFoundException ex) {
			System.out.println("File does not exist");
			System.exit(-1);
		} catch (NumberFormatException ex) {
			System.out.println("Number format exception");
			System.exit(-2);
		}
		
		return pixelLists;
	}
	
	/**
	 * Makes a 2D array of pixels from a 2D ArrayList.
	 * @param pixelLists pixel ArrayList
	 * @return 2D pixel array
	 */
	private static int[][] makePixelArray(ArrayList<String> pixelLists) {
		int[][] pixels = new int[pixelLists.size()][pixelLists.size()];
		
		for (int x = 0; x < pixels.length; ++x) {
			String numbersString = pixelLists.get(x).replaceAll("\\(", "").replaceAll("\\)", "");
			String[] numberStrings = numbersString.split(" ");
			
			for (int y = 0; y < numberStrings.length; ++y)
				pixels[x][y] = Integer.parseInt(numberStrings[y]);
		}
		
		return pixels;
	}
}
