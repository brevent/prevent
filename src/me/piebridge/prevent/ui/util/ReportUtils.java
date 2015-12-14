package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.os.Environment;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/12/14.
 */
public class ReportUtils {

    private ReportUtils() {

    }

    public static void reportBug(Context context) {
        File dir = context.getExternalFilesDir(null);
        File cacheDir = context.getExternalCacheDir();
        if (dir == null || cacheDir == null) {
            return;
        }
        try {
            File path = new File(dir, "logs.zip");
            final ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(path));

            for (File file : cacheDir.listFiles()) {
                zos.putNextEntry(new ZipEntry(file.getName()));
                FileUtils.copyFile(zos, file);
            }

            File xposedLog = new File(Environment.getDataDirectory() + "/data/de.robv.android.xposed.installer/log/error.log");
            if (xposedLog.isFile() && xposedLog.canRead()) {
                zos.putNextEntry(new ZipEntry("xposed.log"));
                FileUtils.copyFile(zos, xposedLog);
            }

            zos.close();
            Runtime.getRuntime().exec("/system/bin/sync");
            EmailUtils.sendZip(context, path, null);
        } catch (IOException e) {
            UILog.d("cannot report bug", e);
        }
    }

    public static void clearReport(Context context) {
        File dir = context.getExternalCacheDir();
        if (dir == null) {
            return;
        }
        for (File file : dir.listFiles()) {
            String path = file.getName();
            if (path.startsWith("system.") || path.startsWith("prevent.")) {
                file.delete();
            }
        }
    }

}
