package me.piebridge.prevent.ui.util;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/8.
 */
public class QQUtils {

    private static final String KEY = "YsQiloC20-IqTq7aK9UnwVHHudfyq667";

    private QQUtils() {

    }

    public static boolean joinQQ(Context context) {
        Intent intent = new Intent();
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.setData(Uri.parse("mqqopensdkapi://bizAgent/qm/qr?url=http%3A%2F%2Fqm.qq.com%2Fcgi-bin%2Fqm%2Fqr%3Ffrom%3Dapp%26p%3Dandroid%26k%3D" + KEY));
        try {
            context.startActivity(intent);
            return true;
        } catch (ActivityNotFoundException e) {
            UILog.d("cannot find qq", e);
            return false;
        }
    }

}
