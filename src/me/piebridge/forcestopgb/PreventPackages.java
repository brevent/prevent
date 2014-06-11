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

public class PreventPackages {

	@SuppressLint("SdCardPath")
	public static final String FORCESTOP = "/data/data/" + PreventPackages.class.getPackage().getName() + "/conf/forcestop.list";

	public static long save(Map<String, Boolean> packages, String suffix) {
		try {
			File file = new File(FORCESTOP + suffix);
			while (file.exists() && System.currentTimeMillis() - file.lastModified() > 3000) {
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
			file.renameTo(new File(FORCESTOP));
		} catch (IOException e) {
			e.printStackTrace();
		}
		return lastModified();
	}

	public static Map<String, Boolean> load() {
		Map<String, Boolean> packages = new HashMap<String, Boolean>();
		try {
			String line;
			File file = new File(FORCESTOP);
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

	public static long lastModified() {
		File file = new File(FORCESTOP);
		if (!file.isFile()) {
			file.delete();
			return 0L;
		} else {
			return file.lastModified();
		}
	}

	public static void ensureDirectory() {
		File parent = new File(FORCESTOP).getParentFile();
		parent.mkdirs();
		FileUtils.setPermissions(parent.getAbsolutePath(), 0777, 1000, 1000);
	}

}
