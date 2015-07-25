package me.piebridge.prevent.ui.util;

import android.os.Environment;
import android.os.FileUtils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

public final class PreventListUtils {

    public static final String PREVENT = Environment.getDataDirectory() + "/data/me.piebridge.forcestopgb/conf/prevent.list";

    private static final int MAX_WAIT = 3000;
    private static final int SINGLE_WAIT = 1000;

    private PreventListUtils() {

    }

    public static void save(Set<String> packages) {
        File lock = new File(PREVENT + ".lock");
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
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(lock));
            for (String key : new TreeSet<String>(packages)) {
                writer.write(key);
                writer.write("\n");
            }
            writer.close();
            lock.renameTo(new File(PREVENT));
            FileUtils.setPermissions(PREVENT, 0644, -1, -1); // NOSONAR
        } catch (IOException e) { // NOSONAR
            // do nothing
        }
    }

    private static Set<String> load(File file) {
        Set<String> packages = new TreeSet<String>();
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
                packages.add(line);
            }
            reader.close();
        } catch (IOException e) { // NOSONAR
            // do nothing
        }
        return packages;
    }

    public static Set<String> load() {
        return load(new File(PREVENT));
    }

}
