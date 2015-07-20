package android.content.pm;

import java.util.ArrayList;

/**
  * @hide
  */
public class PackageParser {

    public static class IntentInfo {
    }

    public static class ActivityIntentInfo extends IntentInfo {
        public Activity activity;
    }

    public static class ServiceIntentInfo extends IntentInfo {
        public Service service;
    }

    public static class Component {
        public Package owner;
    }

    public static class Activity extends Component {
    }

    public static class Service extends Component {
    }

    public static class Package {
        public ApplicationInfo applicationInfo;
        public ArrayList<Activity> activities;
        public ArrayList<Activity> receivers;
    }

}
