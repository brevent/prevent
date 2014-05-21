package me.piebridge.forcestopgb;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import android.annotation.SuppressLint;
import android.os.FileUtils;

public class PackageProvider {

	@SuppressLint("SdCardPath")
	public static final String CONFDIR = "/data/data/me.piebridge.forcestopgb/conf";
	public static final String FORCESTOP = CONFDIR + "/forcestop.list";

	public static long saveToFile(String path, Map<String, Boolean> packages, String suffix) {
		try {
			File file = new File(path + suffix);
			while (file.exists() && System.currentTimeMillis() - file.lastModified() > 10000) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			Set<String> keys = new TreeSet<String>(packages.keySet());
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			for (String key : keys) {
				writer.write(key);
				writer.write("=");
				writer.write(String.valueOf(packages.get(key)));
				writer.write("\n");
			}
			writer.close();
			FileUtils.setPermissions(file.getAbsolutePath(), 0666, -1, -1);
			file.renameTo(new File(path));
		} catch (IOException e) {
			e.printStackTrace();
		}
		return getMTime(path);
	}

	public static Map<String, Boolean> loadFromFile(String path) {
		Map<String, Boolean> packages = new HashMap<String, Boolean>();
		try {
			String line;
			File file = new File(path);
			BufferedReader reader = new BufferedReader(new FileReader(file));
			while ((line = reader.readLine()) != null) {
				line = line.trim();
				String[] components = line.split("=");
				if (components.length > 1) {
					packages.put(components[0], Boolean.valueOf(components[1]));
				} else {
					packages.put(line, Boolean.TRUE);
				}
			}
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return packages;
	}

	public static long getMTime(String path) {
		File file = new File(path);
		if (file != null && file.exists()) {
			return file.lastModified();
		}
		return 0L;
	}

	private static int readContent(File file) {
		if (!file.exists()) {
			return 0;
		}
		if (System.currentTimeMillis() - file.lastModified() > 10000) {
			file.delete();
			return 0;
		}
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			String line = reader.readLine();
			reader.close();
			return Integer.parseInt(line);
		} catch (NumberFormatException e) {
			return 0;
		} catch (IOException e) {
			return 0;
		}
	}

	private static void writeContent(File file, int count) {
		try {
			File tmp = new File(file.getAbsoluteFile() + ".tmp");
			BufferedWriter writer = new BufferedWriter(new FileWriter(tmp));
			writer.write(String.valueOf(count));
			writer.write("\n");
			writer.close();
			FileUtils.setPermissions(tmp.getAbsolutePath(), 0666, -1, -1);
			tmp.renameTo(file);
		} catch (IOException e) {
		}
	}

	public static int increaseCount(String packageName) {
		File file = new File(CONFDIR, packageName);
		int count = readContent(file) + 1;
		writeContent(file, count);
		return count;
	}

	public static int decreaseCount(String packageName) {
		File file = new File(CONFDIR, packageName);
		int count = readContent(file) - 1;
		if (count == 0) {
			file.delete();
		}
		return count;
	}
}
