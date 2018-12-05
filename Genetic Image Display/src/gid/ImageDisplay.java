package gid;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

/**
 * Display to render the image.
 * 
 * @author Gage Davidson
 */
class ImageDisplay extends JPanel {
	
	private static final int DISPLAY_SIZE = 512;
	private static final String DISPLAY_TITLE = "Genetic Image Display";
	
	// range of values a pixel can be
	private final int pixelRange;
	// size of each pixel in pixels
	private final int pixelSize;
	private final int[][] pixels;
	
	ImageDisplay(int pixelRange, int[][] pixels) {
		this.pixelRange = pixelRange;
		pixelSize = (DISPLAY_SIZE / pixels.length);
		this.pixels = pixels;
		
		setPreferredSize(new Dimension(DISPLAY_SIZE, DISPLAY_SIZE));
		
		JFrame frame = new JFrame(DISPLAY_TITLE);
		frame.add(this);
		frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
		
		requestFocus();
	}
	
	/**
	 * Called when repaint() is called on the display.
	 */
	@Override
	public void paintComponent(Graphics g) {
		g.setColor(Color.BLACK);
		g.fillRect(0, 0, getWidth(), getHeight());
		
		drawImage(g);
	}
	
	/**
	 * Draws the image.
	 * @param g Graphics to use
	 */
	private void drawImage(Graphics g) {
		for (int x = 0; x < pixels.length; ++x) {
			for (int y = 0; y < pixels[x].length; ++y) {
				g.setColor(calculateColor(x, y));
				g.fillRect(x * pixelSize, y * pixelSize, pixelSize, pixelSize);
			}
		}
	}
	
	/**
	 * @param x x-coordinate of the pixel
	 * @param y y-coordinate of the pixel
	 * @return color the pixel should be
	 */
	private Color calculateColor(int x, int y) {
		if (pixelRange <= 7) {
			switch (pixels[x][y]) {
			case 0: return Color.RED;
			case 1: return Color.BLUE;
			case 2: return Color.GREEN;
			case 3: return Color.YELLOW;
			case 4: return Color.MAGENTA;
			case 5: return Color.CYAN;
			case 6: return Color.ORANGE;
			case 7: return Color.PINK;
			}
		}
		
		int shade = (int) ((float) 200 / pixelRange * pixels[x][y]) + 55;
		return new Color(shade, shade, shade);
	}
}
