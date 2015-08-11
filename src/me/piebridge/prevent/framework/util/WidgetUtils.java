package me.piebridge.prevent.framework.util;

import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;

import java.util.List;

/**
 * Created by thom on 15/7/31.
 */
public class WidgetUtils {

    private WidgetUtils() {

    }

    public static boolean isWidget(Context context, ComponentName cn) {
        PackageManager packageManager = context.getPackageManager();
        Intent intent = new Intent(AppWidgetManager.ACTION_APPWIDGET_UPDATE);
        intent.setPackage(cn.getPackageName());
        List<ResolveInfo> broadcastReceivers = packageManager.queryBroadcastReceivers(intent, 0);
        final int size = broadcastReceivers == null ? 0 : broadcastReceivers.size();
        for (int i = 0; i < size; ++i) {
            ActivityInfo ai = broadcastReceivers.get(i).activityInfo;
            if ((ai.applicationInfo.flags & ApplicationInfo.FLAG_EXTERNAL_STORAGE) != 0) {
                continue;
            }
            if (new ComponentName(ai.packageName, ai.name).equals(cn)) {
                return true;
            }
        }
        return false;
    }

}
