package me.piebridge.forcestopgb;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.TreeSet;

import android.annotation.SuppressLint;
import android.os.FileUtils;

public class PackageProvider {

	@SuppressLint("SdCardPath")
	public static final String FORCESTOP = "/data/data/me.piebridge.forcestopgb/conf/forcestop.list";

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
}
