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

import android.os.FileUtils;

public class PreventPackages {

    public static final String DATADIR = "/data/data";
    public static final String FORCESTOP = DATADIR + "/" + PreventPackages.class.getPackage().getName() + "/conf/forcestop.list";

    public static synchronized long save(Map<String, Boolean> packages) {
        try {
            String suffix = ".lock";
            File file = new File(FORCESTOP + suffix);
            while (file.exists() && System.currentTimeMillis() - file.lastModified() < 3000) {
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
            if (!file.exists()) {
                return packages;
            }
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
            if (file.exists()) {
                file.delete();
            }
            return 1L;
        } else {
            return file.lastModified();
        }
    }

    static void ensureDirectory() {
        File confdir = new File(FORCESTOP).getParentFile();
        if (!confdir.isDirectory()) {
            if (confdir.exists()) {
                confdir.delete();
            }
            confdir.mkdir();
            save(new HashMap<String, Boolean>());
        }
        FileUtils.setPermissions(confdir.getAbsolutePath(), 0777, -1, -1);
    }

}
