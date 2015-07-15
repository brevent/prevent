package me.piebridge.util;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * Created by thom on 15/7/12.
 */
public class ReflectUtil {

    private ReflectUtil() {

    }

    private static Field getField(Class<?> clazz, boolean canPrivate, String name) {
        if (clazz == null) {
            return null;
        }
        try {
            final Field field = clazz.getDeclaredField(name);
            boolean isPrivate = Modifier.isPrivate(field.getModifiers());
            field.setAccessible(true);
            if (!isPrivate) {
                return field;
            }
            if (canPrivate) {
                return field;
            }
        } catch (NoSuchFieldException e) { // NOSONAR
            // do nothing
        }
        return getField(clazz.getSuperclass(), false, name);
    }

    public static Field getField(Class<?> clazz, String name) {
        return getField(clazz, true, name);
    }

}
