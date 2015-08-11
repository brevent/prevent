package me.piebridge.prevent.framework.util;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.ui.PreventProvider;

/**
 * Created by thom on 15/8/11.
 */
public class LogcatUtils {

    private static final String PATH = "logcat.log";

    private LogcatUtils() {

    }

    public static void sendLogcat(Context context) {
        try {
            File file = new File(context.getCacheDir(), PATH);
            BufferedInputStream stdout = new BufferedInputStream(new FileInputStream(file));
            int length;
            byte[] buffer = new byte[0x400];
            String path = new SimpleDateFormat("yyyyMMdd.HH.mm.ss'.log'", Locale.US).format(new Date());
            ContentResolver contentResolver = context.getContentResolver();
            while ((length = stdout.read(buffer)) != 0) {
                String line = new String(buffer, 0, length);
                Uri uri = PreventProvider.CONTENT_URI.buildUpon().appendQueryParameter("path", path)
                        .appendQueryParameter("log", line).build();
                contentResolver.query(uri, null, null, null, null);
            }
            stdout.close();
        } catch (IOException e) {
            PreventLog.d("exec wrong", e);
        }
    }

    public static void logcat(Context context) {
        try {
            Process process = Runtime.getRuntime().exec("/system/bin/logcat -d -v threadtime");
            BufferedInputStream stdout = new BufferedInputStream(process.getInputStream());
            int length;
            byte[] buffer = new byte[0x1000];
            File file = new File(context.getCacheDir(), PATH);
            FileOutputStream fos = new FileOutputStream(file);
            while ((length = stdout.read(buffer)) != 0) {
                fos.write(buffer, 0, length);
            }
            stdout.close();
            fos.close();
        } catch (IOException e) {
            PreventLog.d("exec wrong", e);
        }
    }

}
