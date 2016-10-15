lib/AndroidHiddenAPI.jar: \
            AndroidHiddenAPI/android/app/*.java \
            AndroidHiddenAPI/android/app/usage/*.java \
            AndroidHiddenAPI/android/content/*.java \
            AndroidHiddenAPI/android/content/pm/*.java \
            AndroidHiddenAPI/android/os/*.java \
            AndroidHiddenAPI/android/view/*.java \
            AndroidHiddenAPI/com/android/server/am/*.java \
            AndroidHiddenAPI/com/android/internal/app/*.java

	javac -cp ${ANDROID_HOME}/platforms/android-25/android.jar \
            AndroidHiddenAPI/android/app/*.java \
            AndroidHiddenAPI/android/app/usage/*.java \
            AndroidHiddenAPI/android/content/*.java \
            AndroidHiddenAPI/android/content/pm/*.java \
            AndroidHiddenAPI/android/os/*.java \
            AndroidHiddenAPI/android/view/*.java \
            AndroidHiddenAPI/com/android/server/am/*.java \
            AndroidHiddenAPI/com/android/internal/app/*.java

	@rm AndroidHiddenAPI/android/app/ActivityManager\$$*.class

	@cd AndroidHiddenAPI; jar -cvf ../lib/AndroidHiddenAPI.jar \
            android/app/*.class \
            android/app/usage/*.class \
            android/content/*.class \
            android/content/pm/*.class \
            android/os/*.class \
            android/view/*.class \
            com/android/server/am/*.class \
            com/android/internal/app/*.class

	@find AndroidHiddenAPI/ -name "*.class" -exec rm {} \;

clean:
	@rm -rf lib/AndroidHiddenAPI.jar
	@find AndroidHiddenAPI/ -name "*.class" -exec rm -fv {} \;
