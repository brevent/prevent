package me.piebridge.prevent.framework.util;

import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProviderInfo;
import android.content.ComponentName;
import android.content.Context;

import java.util.List;

/**
 * Created by thom on 15/7/31.
 */
public class WidgetUtils {

    private WidgetUtils() {

    }

    public static boolean isWidget(Context context, ComponentName cn) {
        List<AppWidgetProviderInfo> widgets = AppWidgetManager.getInstance(context).getInstalledProviders();
        for (AppWidgetProviderInfo widget : widgets) {
            if (widget.provider.equals(cn)) {
                return true;
            }
        }
        return false;
    }

}
