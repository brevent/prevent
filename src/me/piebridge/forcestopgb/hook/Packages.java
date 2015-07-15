package me.piebridge.forcestopgb.hook;

import android.os.Environment;
import android.os.FileUtils;
import android.os.Process;

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
import java.util.concurrent.ConcurrentHashMap;

import me.piebridge.forcestopgb.common.CommonIntent;

final class Packages {

    public static final String FORCESTOP_DEPRECATED = Environment.getDataDirectory() + "/data/me.piebridge.forcestopgb/conf/forcestop.list";;
    public static final String FORCESTOP = Environment.getDataDirectory() + "/system/forcestop.list";

    private Packages() {

    }

    public static void save(Map<String, Boolean> packages) {
        android.util.Log.d(CommonIntent.TAG, "save packages");
        File lock = new File(FORCESTOP + ".lock");
        while (lock.exists() && System.currentTimeMillis() - lock.lastModified() < 3000) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) { // NOSONAR
                // do nothing
            }
        }
        Set<String> keys = new TreeSet<String>(packages.keySet());
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(lock));
            for (String key : keys) {
                writer.write(key);
                writer.write("\n");
            }
            writer.close();
            lock.renameTo(new File(FORCESTOP));
        } catch (IOException e) { // NOSONAR
            // do nothing
        }
    }

    private static Map<String, Boolean> load(File file) {
        Map<String, Boolean> packages = new ConcurrentHashMap<String, Boolean>();
        if (!file.exists()) {
            return packages;
        }
        try {
            String line;
            BufferedReader reader = new BufferedReader(new FileReader(file));
            while ((line = reader.readLine()) != null) {
                int index = line.indexOf('=');
                if (index != -1) {
                    line = line.substring(0, index);
                }
                line = line.trim();
                packages.put(line, Boolean.TRUE);
            }
            reader.close();
        } catch (IOException e) { // NOSONAR
            // do nothing
        }
        return packages;
    }

    public static Map<String, Boolean> load() {
        File fileDeprecated = new File(FORCESTOP_DEPRECATED);
        Map<String, Boolean> packages;
        if (fileDeprecated.isFile() && fileDeprecated.canRead()) {
            packages = load(fileDeprecated);
            save(packages);
            fileDeprecated.delete();
        } else {
            packages = load(new File(FORCESTOP));
        }
        return packages;
    }

}
