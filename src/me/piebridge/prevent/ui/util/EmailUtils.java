package me.piebridge.prevent.ui.util;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;

import java.io.File;
import java.util.Locale;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/5.
 */
public class EmailUtils {

    private EmailUtils() {

    }

    public static String getSubject(Context context) {
        StringBuilder subject = new StringBuilder();
        subject.append(context.getString(R.string.app_name));
        subject.append(" ");
        subject.append(BuildConfig.VERSION_NAME);
        subject.append("(Android ");
        subject.append(Locale.getDefault().toString());
        subject.append("-");
        subject.append(Build.VERSION.RELEASE);
        subject.append(")");
        return subject.toString();
    }

    public static void sendEmail(Context context, String content) {
        Intent intent = new Intent(Intent.ACTION_SENDTO, Uri.parse("mailto:liudongmiao@gmail.com"));
        intent.putExtra(Intent.EXTRA_SUBJECT, getSubject(context));
        if (content != null) {
            intent.putExtra(Intent.EXTRA_TEXT, content);
        }
        try {
            context.startActivity(intent);
        } catch (ActivityNotFoundException e) {
            UILog.d("cannot send email", e);
        }
    }

    public static void sendZip(Context context, File path, String content) {
        Intent intent = new Intent(Intent.ACTION_SEND);
        intent.addCategory(Intent.CATEGORY_DEFAULT);
        intent.setType("application/zip");
        intent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(path));
        intent.putExtra(Intent.EXTRA_SUBJECT, EmailUtils.getSubject(context));
        intent.putExtra(Intent.EXTRA_TEXT, content);
        intent.putExtra(Intent.EXTRA_EMAIL, new String[]{"liudongmiao@gmail.com"});
        context.startActivity(intent);
    }

}
