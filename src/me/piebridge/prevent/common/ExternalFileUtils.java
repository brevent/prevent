package me.piebridge.prevent.common;

import android.content.Context;

import java.io.File;

/**
 * Created by thom on 16/2/11.
 */
public class ExternalFileUtils {

    private ExternalFileUtils() {

    }

    public static File[] getExternalFilesDirs(Context context) {
        File[] files;
        files = context.getExternalFilesDirs(null);
        if (files == null) {
            files = new File[0];
        }
        return files;
    }

}
