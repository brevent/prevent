package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.os.Build;
import android.os.Environment;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.ui.UILog;

public final class PreventListUtils {

    public static final String PREVENT = Environment.getDataDirectory() + "/data/" + BuildConfig.APPLICATION_ID + "/conf/prevent.list";
    public static final String PREVENT_DEPRECATED = Environment.getDataDirectory() + "/data/" + BuildConfig.APPLICATION_ID + "/conf/forcestop.list";

    private static final int MAX_WAIT = 3000;
    private static final int SINGLE_WAIT = 1000;

    private PreventListUtils() {

    }

    private static File[] getExternalFilesDirs(Context context) {
        File[] files;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            files = context.getExternalFilesDirs(null);
            if (files == null) {
                files = new File[0];
            }
        } else {
            files = new File[]{context.getExternalFilesDir(null)};
        }
        return files;
    }

    public static synchronized void save(Context context, Set<String> packages) {
        save(PREVENT, packages);
        for (File file : getExternalFilesDirs(context)) {
            if (file != null) {
                save(new File(file, "prevent.list").getAbsolutePath(), packages);
            }
        }
    }

    private static synchronized void save(String path, Set<String> packages) {
        File lock = new File(path + ".lock");
        File conf = lock.getParentFile();
        if (conf.isFile()) {
            conf.delete();
        }
        if (!conf.isDirectory()) {
            conf.mkdir();
        }
        while (lock.exists() && System.currentTimeMillis() - lock.lastModified() < MAX_WAIT) {
            try {
                Thread.sleep(SINGLE_WAIT);
            } catch (InterruptedException e) { // NOSONAR
                // do nothing
            }
        }
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(lock));
            for (String key : packages) {
                writer.write(key);
                writer.write("\n");
            }
            writer.close();
            lock.renameTo(new File(path));
        } catch (IOException e) {
            UILog.e("cannot save " + path, e);
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
        } catch (IOException e) {
            UILog.e("cannot load " + file.getAbsolutePath(), e);
        }
        return packages;
    }

    private static void loadExternal(Set<String> packages, Context context) {
        for (File file : getExternalFilesDirs(context)) {
            if (file != null) {
                packages.addAll(load(new File(file, "prevent.list")));
                if (!packages.isEmpty()) {
                    break;
                }
            }
        }
    }

    public static Set<String> load(Context context) {
        File fileDeprecated = new File(PREVENT_DEPRECATED);
        Set<String> packages = load(new File(PREVENT));
        if (context != null && packages.isEmpty()) {
            loadExternal(packages, context);
        }
        if (fileDeprecated.isFile() && fileDeprecated.canWrite()) {
            UILog.d("migrate packages");
            packages.addAll(load(fileDeprecated));
            fileDeprecated.delete();
        }
        return packages;
    }

}
