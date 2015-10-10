package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Environment;
import android.preference.PreferenceManager;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.UILog;

public final class PreventListUtils {

    public static final String PREVENT = Environment.getDataDirectory() + "/data/" + BuildConfig.APPLICATION_ID + "/conf/prevent.list";
    public static final String PREVENT_DEPRECATED = Environment.getDataDirectory() + "/data/" + BuildConfig.APPLICATION_ID + "/conf/forcestop.list";

    private static final int MAX_WAIT = 3000;
    private static final int SINGLE_WAIT = 1000;

    private PreventListUtils() {

    }

    public static File[] getExternalFilesDirs(Context context) {
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
        UILog.i("update prevents: " + packages.size());
        String message = context.getString(R.string.updated_prevents, packages.size());
        Toast.makeText(context, message, Toast.LENGTH_SHORT).show();
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        boolean backup = false;
        try {
            backup = sp.getBoolean(PreventIntent.BACKUP_PREVENT_LIST, false);
        } catch (ClassCastException e) {
            UILog.d("invalid value for " + PreventIntent.BACKUP_PREVENT_LIST, e);
            sp.edit().putBoolean(PreventIntent.BACKUP_PREVENT_LIST, false).apply();
        }
        for (File dir : getExternalFilesDirs(context)) {
            if (dir == null) {
                continue;
            }
            File file = new File(dir, "prevent.list");
            if (backup) {
                save(file.getAbsolutePath(), packages);
            } else if (file.exists()) {
                file.delete();
            }
        }
    }

    public static void saveIfNeeded(Context context, Set<String> packages) {
        if (!new File(PREVENT).exists()) {
            save(context, packages);
        }
    }

    private static void save(String path, Set<String> packages) {
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
