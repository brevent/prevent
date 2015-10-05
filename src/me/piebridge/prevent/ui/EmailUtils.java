package me.piebridge.prevent.ui;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;

import java.util.Locale;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;

/**
 * Created by thom on 15/10/5.
 */
public class EmailUtils {

    private EmailUtils() {

    }

    public static void sendEmail(Context context, String content) {
        StringBuilder subject = new StringBuilder();
        subject.append(context.getString(R.string.app_name));
        subject.append(" ");
        subject.append(BuildConfig.VERSION_NAME);
        subject.append("(Android ");
        subject.append(Locale.getDefault().toString());
        subject.append("-");
        subject.append(Build.VERSION.RELEASE);
        subject.append(")");
        Intent intent = new Intent(Intent.ACTION_SENDTO, Uri.parse("mailto:liudongmiao@gmail.com"));
        intent.putExtra(Intent.EXTRA_SUBJECT, subject.toString());
        if (content != null) {
            intent.putExtra(Intent.EXTRA_TEXT, content);
        }
        try {
            context.startActivity(intent);
        } catch (ActivityNotFoundException e) {
            UILog.d("cannot send email", e);
        }
    }

}
