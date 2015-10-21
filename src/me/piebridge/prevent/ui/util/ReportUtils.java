package me.piebridge.prevent.ui.util;

import android.annotation.SuppressLint;
import android.content.Context;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/21.
 */
public class ReportUtils {

    private ReportUtils() {

    }

    private static void copyInputStream(ZipOutputStream zos, File file) throws IOException {
        byte[] buffer = new byte[0x1000];
        InputStream is = new FileInputStream(file);
        int length;
        while ((length = is.read(buffer)) > 0) {
            zos.write(buffer, 0, length);
        }
        zos.flush();
        is.close();
    }

    public static void reportBug(Context context, String content) {
        File dir = context.getExternalFilesDir(null);
        if (dir == null) {
            return;
        }
        try {
            File path = new File(dir, "logs.zip");
            final ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(path));

            for (File file : context.getExternalCacheDir().listFiles()) {
                zos.putNextEntry(new ZipEntry(file.getName()));
                copyInputStream(zos, file);
            }

            @SuppressLint("SdCardPath")
            File xposedLog = new File("/data/data/de.robv.android.xposed.installer/log/error.log");
            if (xposedLog.isFile() && xposedLog.canRead()) {
                zos.putNextEntry(new ZipEntry("xposed.log"));
                copyInputStream(zos, xposedLog);
            }

            zos.close();
            Runtime.getRuntime().exec("/system/bin/sync");
            EmailUtils.sendZip(context, path, content);
        } catch (IOException e) {
            UILog.d("cannot report bug", e);
        }
    }

}
