package me.piebridge.forcestopgb.common;

import android.os.Environment;
import android.os.FileUtils;
import android.util.Log;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

import me.piebridge.forcestopgb.common.CommonIntent;

public final class Packages {

    public static final String FORCESTOP = Environment.getDataDirectory() + "/data/me.piebridge.forcestopgb/conf/forcestop.list";
    public static final String FORCESTOP_DEPRECATED = Environment.getDataDirectory() + "/system/forcestop.list";

    private static final int MAX_WAIT = 3000;
    private static final int SINGLE_WAIT = 1000;

    private Packages() {

    }

    public static void save(Map<String, Boolean> packages) {
        File lock = new File(FORCESTOP + ".lock");
        File conf = lock.getParentFile();
        if (conf.isFile()) {
            conf.delete();
        }
        if (!conf.isDirectory()) {
            conf.mkdir();
        }
        FileUtils.setPermissions(conf.getAbsolutePath(), 0755, -1, -1); // NOSONAR
        while (lock.exists() && System.currentTimeMillis() - lock.lastModified() < MAX_WAIT) {
            try {
                Thread.sleep(SINGLE_WAIT);
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
            FileUtils.setPermissions(FORCESTOP, 0644, -1, -1); // NOSONAR
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
        Map<String, Boolean> packages = load(new File(FORCESTOP));
        if (fileDeprecated.isFile() && fileDeprecated.canWrite()) {
            Log.d(CommonIntent.TAG, "migrate packages");
            packages.putAll(load(fileDeprecated));
            fileDeprecated.delete();
        }
        return packages;
    }

}
